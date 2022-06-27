with Ada.Text_IO;   use Ada.Text_IO;
with ada.Streams;   use ada.Streams;

with Unchecked_Conversion;
with Bloom_Filters;

procedure Test_Bloom is
   type Key_Type is mod 2 ** 32;

   subtype Key_Buffer is Stream_Element_Array (1 .. 4);

   function To_Stream_Array is
     new Unchecked_Conversion (Source => Key_Type,
                               Target => Key_Buffer);

   function To_Stream_Array (X : String) return Stream_Element_Array
   is
      Y : constant String (1 .. X'Length) := X;
   begin
      return Result : Stream_Element_Array (1 .. X'Length) do
         for I in y'Range loop
            Result (Stream_Element_Offset (I)) :=
              Stream_Element (Character'Pos (Y (I)));
         end loop;
      end return;
   end To_Stream_Array;

   package Integer_Bloom is
     new Bloom_Filters (Key_Type);

   package String_Bloom is
     new Bloom_Filters (String);


   Filter : Integer_Bloom.Bloom_Filter := Integer_Bloom.Create (12, 4);

   Filter_S : String_Bloom.Bloom_Filter := String_Bloom.Create (22, 6);
begin
   Integer_Bloom.Add (Filter, 7);
   Integer_Bloom.Add (Filter, 12345678);
   Integer_Bloom.Add (Filter, -33);

   Put_Line (Boolean'Image (Integer_Bloom.Contains (Filter, 7)));
   Put_Line (Boolean'Image (not Integer_Bloom.Contains (Filter, 17)));

   Put_Line (Boolean'Image (not Integer_Bloom.Contains (Filter, 32471)));

   Put_Line (Boolean'Image (Integer_Bloom.Contains (Filter, -33)));

   String_Bloom.Add (Filter_S, "pippo");
   String_Bloom.Add (Filter_S, "pluto");

   Put_Line (Boolean'Image (not String_Bloom.Contains (Filter_S, "topolino")));

   Put_Line (Boolean'Image (String_Bloom.Contains (Filter_S, "pippo")));

end Test_Bloom;
