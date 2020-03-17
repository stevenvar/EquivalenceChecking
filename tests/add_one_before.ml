let f = fun x -> x + 2 ;;

let h = fun y -> f y + 3 ;;

let y x = h x ;; 

let g x = y x ;; 

let main = g 45 ;;
 
