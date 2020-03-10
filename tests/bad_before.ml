let f x = x + 1 ;;

   
let k x = 3  *  x + f x ;;

let h y =
    let g = fun z -> z*z in 
    k y + f (k y) ;;

let u = h 12 ;;
