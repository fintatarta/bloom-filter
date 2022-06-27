pragma Ada_2012;

with Ada.Numerics.Elementary_Functions;

with GNAT.SHA512;

with Stream_Array_Extractors;

package body Bloom_Filters is
   type Hash_Array is array (Map_Index range <>) of Hash_Type;

   ----------------
   -- Get_Hashes --
   ----------------

   function Get_Hashes (Item         : Key_Type;
                        N_Hashes     : Map_Index;
                        Bit_Per_Hash : Positive)
                        return Hash_Array
   is
      use Ada.Streams;
      use Stream_Array_Extractors;

      function Basic_Hash (Item : Key_Type) return Stream_Element_Array
      is
         --  Entry_Buffer_Size : constant Stream_Element_Count :=
         --                        Item'Size / Stream_Element'Size;
         --
         --  subtype Entry_Buffer is Stream_Element_Array
         --  (1 .. Entry_Buffer_Size);
         --
         --  function To_Stream_Array is
         --    new Ada.Unchecked_Conversion (Source => Key_Type,
         --                                  Target => Entry_Buffer);

      begin
         return GNAT.SHA512.Digest (To_Stream_Array (Item));
      end Basic_Hash;

      Extractor : Bit_Extractor := Create (Basic_Hash (Item));

      Result : Hash_Array (1 .. N_Hashes);

   begin
      declare
         Tmp : Unsigned_128;
      begin
         for K in Result'Range loop
            Extract (From   => Extractor,
                     N_Bit  => Bit_Per_Hash,
                     Result => Tmp);

            Result (K) := Hash_Type (Tmp);
         end loop;
      end;
      return Result;
   end Get_Hashes;

   ------------
   -- Create --
   ------------

   function Create
     (Table_Size : Positive;
      N_Hashes   : Positive) return Bloom_Filter
   is
      procedure Next_Pow_2 (X        : Positive;
                            Pow_Of_2 : out Positive;
                            Log_2    : out Natural)
        with
          Pre => X >= 2,
          Post =>
            Pow_Of_2 >= X
            and then Pow_Of_2 / 2 < X
            and then Pow_Of_2 = 2 ** Log_2;

      procedure Next_Pow_2 (X        : Positive;
                            Pow_Of_2 : out Positive;
                            Log_2    : out Natural)
      is
      begin
         Pow_Of_2 := 1;
         Log_2 := 0;

         while Pow_Of_2 * 2 <= X loop
            Pow_Of_2 := Pow_Of_2 * 2;
            Log_2 := Log_2 + 1;
         end loop;

         pragma Assert (Pow_Of_2 * 2 > X and then Pow_Of_2 <= X);

         if Pow_Of_2 < X then
            Pow_Of_2 := Pow_Of_2 * 2;
            Log_2 := Log_2 + 1;
         end if;
      end Next_Pow_2;

      Pow_2 : Positive;
      Log_2 : Positive;
   begin
      Next_Pow_2 (X        => Table_Size,
                  Pow_Of_2 => Pow_2,
                  Log_2    => Log_2);

      return Bloom_Filter'(N_Hashes     => Map_Index (N_Hashes),
                           Max_Hash     => Hash_Type (Pow_2 - 1),
                           Maps         => (others => (others => False)),
                           Bit_Per_Hash => Log_2);
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Expected_N_Items    : Positive;
      False_Positive_P : Probability)
      return Bloom_Filter
   is
      use Ada.Numerics.Elementary_Functions;

      N_Hash : constant Float :=
                 Float'Ceiling (-Log (Float (False_Positive_P)) / Log (2.0));

      Table_Size : constant Positive :=
                     Positive (N_Hash * Float (Expected_N_Items) / Log (2.0));
   begin
      return Create (Table_Size => Table_Size,
                     N_Hashes   => Positive (N_Hash));
   end Create;

   ---------
   -- Add --
   ---------

   procedure Add (Filter : in out Bloom_Filter; Item : Key_Type) is
      Hashes : constant Hash_Array :=
                 Get_Hashes (Item         => Item,
                             N_Hashes     => Filter.N_Hashes,
                             Bit_Per_Hash => Filter.Bit_Per_Hash);
   begin
      for Map in Hashes'Range loop
         Filter.Maps (Map, Hashes (Map)) := True;
      end loop;
   end Add;

   --------------
   -- Contains --
   --------------

   function Contains (Filter : Bloom_Filter; Item : Key_Type) return Boolean
   is
      Hashes : constant Hash_Array :=
                 Get_Hashes (Item         => Item,
                             N_Hashes     => Filter.N_Hashes,
                             Bit_Per_Hash => Filter.Bit_Per_Hash);
   begin
      for Map in Hashes'Range loop
         if not Filter.Maps (Map, Hashes (Map)) then
            return False;
         end if;
      end loop;

      return True;
   end Contains;

end Bloom_Filters;
