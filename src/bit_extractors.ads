generic
   type Entry_Type is mod <>;

   type Index_Type is range <>;

   type Buffer_Type is array (Index_Type range <>) of Entry_Type;
package Bit_Extractors is
   type Unsigned_128 is mod 2 ** 128;

   type Bit_Extractor (<>) is private;

   function Create (Data : Buffer_Type)
                    return Bit_Extractor
     with
       Post =>
         Remaining (Create'Result) = Entry_Type'Size * Data'Length;

   procedure Extract (From   : in out Bit_Extractor;
                      N_Bit  : Positive;
                      Result : out Unsigned_128)
     with
       Pre => N_Bit <= Remaining (From),
       Post => Remaining (From) = Remaining (From)'Old - N_Bit;

   function Remaining (Extractor : Bit_Extractor) return Natural;
private
   type Bit_Extractor (Size : Index_Type) is
      record
         Buffer           : Buffer_Type (1 .. Size);
         Cursor           : Index_Type;
         Working_Area     : Unsigned_128;
         Bit_In_Work_Area : Natural;
      end record
     with
       Dynamic_Predicate =>
         Bit_Extractor.Working_Area < 2 ** Bit_Extractor.Bit_In_Work_Area
         and then Bit_Extractor.Cursor >= Bit_Extractor.Buffer'First
         and then Bit_Extractor.Cursor <= Bit_Extractor.Buffer'Last + 1;

end Bit_Extractors;
