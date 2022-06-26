pragma Ada_2012;

with Ada.Storage_IO;
with Ada.Streams;
with Ada.Unchecked_Conversion;

with GNAT.SHA512;

with Bit_Extractors;

package body Bloom_Filters is
   package Stream_Array_Extractors is
     new Bit_Extractors (Entry_Type  => Ada.Streams.Stream_Element,
                         Index_Type  => Ada.Streams.Stream_Element_Offset,
                         Buffer_Type => Ada.Streams.Stream_Element_Array);

   package Key_Storage_Io is
     new Ada.Storage_IO (Key_Type);

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
         subtype Intermediate_Array is
           Stream_Element_Array (1 .. Key_Storage_Io.Buffer_Type'Length);

         function To_Stream_Array is
           new Ada.Unchecked_Conversion (Source => Key_Storage_Io.Buffer_Type,
                                         Target => Intermediate_Array);

         Buffer : Key_Storage_Io.Buffer_Type;
      begin
         Key_Storage_Io.Write (Buffer => Buffer,
                               Item   => Item);

         return GNAT.SHA512.Digest (To_Stream_Array (Buffer));
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
     (Expected_Fill : Fill_Ratio; False_Positive_Prob : Probability)
      return Bloom_Filter
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Create unimplemented");
      return raise Program_Error with "Unimplemented function Create";
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
