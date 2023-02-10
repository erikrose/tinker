open OUnit2

let () =
  run_test_tt_main (test_list [Test_general.tests; Test_unify.tests])
