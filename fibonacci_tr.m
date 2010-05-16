:- module fibonacci_tr.

:- interface.

:- func fibonacci(int) = int.
:- mode fibonacci(in) = out is det.

:- implementation.

:- import_module int.
 
fibonacci(N) = F  :-   F = fibo_tr( N, 1, 0). 

/* tail recursive private function */

:- func fibo_tr(int, int, int) = int.
:- mode fibo_tr(in, in, in) = out is det.

fibo_tr(N, Next, Result) = F   :-   

      ( N = 0 -> F = Result 

      ;
        /* else */
        F = fibo_tr(N - 1, Result + Next, Next) 
      ).
