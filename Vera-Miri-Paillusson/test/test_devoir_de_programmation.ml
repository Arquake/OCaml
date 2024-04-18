open OUnit2
open Lib

let query_sum = Lib.Sum.create [ 1; 3; -2; 8; -7; 6; -5; 1 ]
let updated = Sum.update query_sum 8 4

let test_sum_query1 _ =
  assert_equal "10" (Sum.to_string (Sum.query query_sum 0 3))

let test_sum_query2 _ =
  assert_equal "0" (Sum.to_string (Sum.query query_sum 2 6))

let test_sum_query3 _ =
  assert_equal "-5" (Sum.to_string (Sum.query query_sum 4 7))

let test_sum_query4 _ =
  assert_equal "5" (Sum.to_string (Sum.query query_sum 0 7))

let test_sum_update1 _ =
  assert_equal "10" (Sum.to_string (Sum.query updated 0 3))

let test_sum_update2 _ =
  assert_equal "15" (Sum.to_string (Sum.query updated 2 6))

let test_sum_update3 _ =
  assert_equal "10" (Sum.to_string (Sum.query updated 4 7))

let test_sum_update4 _ =
  assert_equal "20" (Sum.to_string (Sum.query updated 0 7))

let query_max = MaxOcc.create [ 1; 1; -2; 8; -7; 6; -5; 8 ]

let test_max_query1 _ =
  assert_equal "(8, 1)" (MaxOcc.to_string (MaxOcc.query query_max 0 3))

let test_max_query2 _ =
  assert_equal "(8, 1)" (MaxOcc.to_string (MaxOcc.query query_max 2 6))

let test_max_query3 _ =
  assert_equal "(6, 1)" (MaxOcc.to_string (MaxOcc.query query_max 4 6))

let test_max_query4 _ =
  assert_equal "(8, 2)" (MaxOcc.to_string (MaxOcc.query query_max 0 7))

let query_mss = MaxSubSeg.create [ 1; 3; -2; 8; 7; 6; -5; 1 ]

let test_mss_query1 _ =
  assert_equal "10" (MaxSubSeg.to_string (MaxSubSeg.query query_mss 0 3))

let test_mss_query2 _ =
  assert_equal "21" (MaxSubSeg.to_string (MaxSubSeg.query query_mss 2 6))

let test_mss_query3 _ =
  assert_equal "13" (MaxSubSeg.to_string (MaxSubSeg.query query_mss 4 7))

let test_mss_query4 _ =
  assert_equal "23" (MaxSubSeg.to_string (MaxSubSeg.query query_mss 0 7))

let suite =
  "TestQueries"
  >::: [
         "test_sum_query1" >:: test_sum_query1;
         "test_sum_query2" >:: test_sum_query2;
         "test_sum_query3" >:: test_sum_query3;
         "test_sum_query4" >:: test_sum_query4;
         "test_sum_update1" >:: test_sum_update1;
         "test_sum_update2" >:: test_sum_update2;
         "test_sum_update3" >:: test_sum_update3;
         "test_sum_update4" >:: test_sum_update4;
         "test_max_query1" >:: test_max_query1;
         "test_max_query2" >:: test_max_query2;
         "test_max_query3" >:: test_max_query3;
         "test_max_query4" >:: test_max_query4;
         "test_mss_query1" >:: test_mss_query1;
         "test_mss_query2" >:: test_mss_query2;
         "test_mss_query3" >:: test_mss_query3;
         "test_mss_query4" >:: test_mss_query4;
       ]

let () = run_test_tt_main suite
