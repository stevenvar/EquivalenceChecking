let rec fibo x = if x == 0 then 1
              else if x == 1 then 1 
              else fibo (x-1) + fibo (x-2)  ;;

let h () = fibo 12;;
  
