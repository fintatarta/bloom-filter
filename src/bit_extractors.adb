pragma Ada_2012;
package body Bit_Extractors is

   ------------
   -- Create --
   ------------

   function Create (Data : Buffer_Type) return Bit_Extractor is
   begin
      return Bit_Extractor'(Size             => Data'Length,
                            Buffer           => Data,
                            Cursor           => 1,
                            Working_Area     => 0,
                            Bit_In_Work_Area => 0);
   end Create;

   -------------
   -- Extract --
   -------------

   procedure Extract
     (From : in out Bit_Extractor; N_Bit : Positive; Result : out Unsigned_128)
   is
   begin
      if Remaining (From) < N_Bit then
         raise Constraint_Error;
      end if;

      while
        From.Bit_In_Work_Area < N_Bit
        and then From.Cursor <= From.Buffer'Last
      loop
         From.Working_Area :=
           Entry_Type'Modulus * From.Working_Area
             + Unsigned_128 (From.Buffer (From.Cursor));

         From.Bit_In_Work_Area :=
           From.Bit_In_Work_Area + Entry_Type'Size;

         From.Cursor := From.Cursor + 1;
      end loop;

      pragma Assert (From.Bit_In_Work_Area >= N_Bit);

      if From.Bit_In_Work_Area < N_Bit then
         raise Program_Error;

         --  This should never happen because of the if at the beginning
         --  but let's be difensive
      end if;

      declare
         M : constant Unsigned_128 := 2 ** (From.Bit_In_Work_Area - N_Bit);
      begin
         Result := From.Working_Area / M;

         From.Working_Area := From.Working_Area mod M;

         From.Bit_In_Work_Area := From.Bit_In_Work_Area - N_Bit;
      end;
   end Extract;

   ---------------
   -- Remaining --
   ---------------

   function Remaining (Extractor : Bit_Extractor) return Natural is
   begin
      return Extractor.Bit_In_Work_Area
        + Entry_Type'Stream_Size
        * Natural (Extractor.Buffer'Last - Extractor.Cursor + 1);
   end Remaining;

end Bit_Extractors;
