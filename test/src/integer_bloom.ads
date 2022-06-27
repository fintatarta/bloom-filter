with Bloom_Filters;
package Integer_Bloom  is
   pragma SPARK_Mode (On);

   package pippo is
     new Bloom_Filters (Integer);
end Integer_Bloom;
