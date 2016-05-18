(* Look at a set of numbers, and determine their likely distribution relative to other numbers.
   That is to say, if the function is given 10 values and they're sorted smallest-to-largest,
     then the change of things <= the 3rd value is roughly 2/10. 
     
   An example - Say values = [0; 1; ... 99; 100].
   The return = [(0, 0.0); (1, 0.01); (2, 0.02); ... (98, 0.98); (99, 0.99), (100, 1.0)]

   An example - Say values = [7; 12; 14; 45; 46; 99]
   The return = [(7, 0.16666667); (12, 0.333333333); (14, 0.5); (45, 0.6666666667); (46, 0.833333333); (99, 1.0)]
*)
let uniformDistribution values =
    let n = float (Seq.length values)
    Seq.mapi (fun i value -> (value, (float (i + 1) / n))) values
