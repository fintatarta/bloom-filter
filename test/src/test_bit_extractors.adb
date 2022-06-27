with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Streams;            use Ada.Streams;

with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;

with Stream_Array_Extractors;

procedure Test_Bit_Extractors is
   function To_Binary (X : Natural; Len : Positive) return String
   is
      Tmp : Natural := X;
   begin
      return Result : String (1 .. Len)  do
         for I in reverse Result'Range loop
            Result (I) := Character'Val (Character'Pos ('0') + Tmp mod 2);
            Tmp := Tmp / 2;
         end loop;

         if Tmp /= 0 then
            raise Constraint_Error with Tmp'Image;
         end if;
      end return;
   end To_Binary;

   function To_Binary (X : Stream_Element_Array) return String
   is
      use Ada.Strings.Unbounded;

      Result : Unbounded_String;
   begin
      for I in X'Range loop
         Result := Result & To_Binary (Integer (X (I)), 8);
      end loop;

      return To_String (Result);
   end To_Binary;

   procedure Do_Test (Data  : Stream_Element_Array;
                      N_Bit : Positive)
   is
      use Stream_Array_Extractors;
      use Ada.Strings.Unbounded;

      Extractor : Bit_Extractor := Create (Data);
      Expected : constant String := To_Binary (Data);
      Accumulator : Unbounded_String;
      Tmp : Unsigned_128;
   begin
      while Remaining (Extractor) >= N_Bit loop
         Extract (From   => Extractor,
                  N_Bit  => N_Bit,
                  Result => Tmp);

         -- Put_Line (Tmp'Image & " " & To_Binary (Integer (Tmp), N_Bit));

         Accumulator := Accumulator & To_Binary (Integer (Tmp), N_Bit);
      end loop;

      declare
         use Ada.Strings;

         S : constant String := To_String (Accumulator);
         E : constant String := Fixed.Head (Expected, S'Length);
         R : constant Natural :=  (Data'Length * 8) mod N_Bit;
      begin
         if False then
            Put_Line (Expected);
            Put_Line (S);
            Put_Line (E);
         end if;

         Put_Line (Boolean'Image ((S = E) and (R = Remaining (Extractor))));
      end;
   end Do_Test;


   procedure Test_To_Binary is
      Success : Boolean := True;
   begin
      Success := Success and (To_Binary (3, 4) = "0011");
      Success := Success and (To_Binary (3, 2) = "11");
      Success := Success and (To_Binary (16#fa#, 9) = "011111010");

      Put_Line ("Test to binary: " & Success'Image);
   end Test_To_Binary;

   procedure Test_Extraction is
      A   : constant Stream_Element_Array := (13, 25, 41, 79, 115, 250, 7);
   begin
      Do_Test (A, 11);
      Do_Test (A, 3);
      Do_Test (A, 15);
      Do_Test (A, 8);
      Do_Test (A, 24);
   end Test_Extraction;
begin
   Test_Extraction;
end Test_Bit_Extractors;
