with Ada.Text_IO; use Ada.Text_IO;
with Integer_Bloom;

procedure Test_Bloom is
   use Integer_Bloom;
   Filter : Bloom_Filter := Create (12, 4);
begin
   Add (Filter, 7);
   Add (Filter, 12345678);
   Add (Filter, -33);

   Put_Line (Boolean'Image (Contains (Filter, 7)));
   Put_Line (Boolean'Image (not Contains (Filter, 17)));

   Put_Line (Boolean'Image (not Contains (Filter, 32471)));

   Put_Line (Boolean'Image (Contains (Filter, -33)));
end Test_Bloom;
