let f = fun z -> fun x -> x + z ;;

let h = fun y -> ((f 2) y) + 3 ;;
