let f x = fun y -> x + y ;;

let g x = x ;;
  
let rec k g =
  let fa = f in
  let y = 2 in
  ((fa y 9) + (g 10 10));;

  

