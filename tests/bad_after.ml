let f x = x + 1 ;;

let g x = 3 * x + f x  ;;

let h y = 
    let g = fun z -> z * z in
    g y + f (g y) ;;

let u = h 12;;
