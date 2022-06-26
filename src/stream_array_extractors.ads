with Ada.Streams;
with Bit_Extractors;

package Stream_Array_Extractors is
  new Bit_Extractors (Entry_Type  => Ada.Streams.Stream_Element,
                      Index_Type  => Ada.Streams.Stream_Element_Offset,
                      Buffer_Type => Ada.Streams.Stream_Element_Array);
