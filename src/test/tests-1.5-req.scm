(add-tests-with-string-output "fx+"
  ((fx+ 1 2) => "3\n")
  ((fx+ 1 -2) => "-1\n")
  ((fx+ -1 2) => "1\n")
  ((fx+ -1 -2) => "-3\n")
  ((fx+ 536870911 -1) => "536870910\n")
  ((fx+ 536870910 1) => "536870911\n")
  ((fx+ -536870912 1) => "-536870911\n")
  ((fx+ -536870911 -1) => "-536870912\n")
  ((fx+ 536870911 -536870912) => "-1\n")
  ((fx+ 1 (fx+ 2 3)) => "6\n")
  ((fx+ 1 (fx+ 2 -3)) => "0\n")
  ((fx+ 1 (fx+ -2 3)) => "2\n")
  ((fx+ 1 (fx+ -2 -3)) => "-4\n")
  ((fx+ -1 (fx+ 2 3)) => "4\n")
  ((fx+ -1 (fx+ 2 -3)) => "-2\n")
  ((fx+ -1 (fx+ -2 3)) => "0\n")
  ((fx+ -1 (fx+ -2 -3)) => "-6\n")
  ((fx+ (fx+ 1 2) 3) => "6\n")
  ((fx+ (fx+ 1 2) -3) => "0\n")
  ((fx+ (fx+ 1 -2) 3) => "2\n")
  ((fx+ (fx+ 1 -2) -3) => "-4\n")
  ((fx+ (fx+ -1 2) 3) => "4\n")
  ((fx+ (fx+ -1 2) -3) => "-2\n")
  ((fx+ (fx+ -1 -2) 3) => "0\n")
  ((fx+ (fx+ -1 -2) -3) => "-6\n")
  ((fx+ (fx+ (fx+ (fx+ (fx+ (fx+ (fx+ (fx+ 1 2) 3) 4) 5) 6) 7) 8) 9) => "45\n")
  ((fx+ 1 (fx+ 2 (fx+ 3 (fx+ 4 (fx+ 5 (fx+ 6 (fx+ 7 (fx+ 8 9)))))))) => "45\n")
  ((fx+ (fx+ 1 2) (fx+ 3 (fx+ 2 2))) => "10\n")
)
 
(add-tests-with-string-output "fx-"
  ((fx- 1 2) => "-1\n")
  ((fx- 1 -2) => "3\n")
  ((fx- -1 2) => "-3\n")
  ((fx- -1 -2) => "1\n")
  ((fx- 536870910 -1) => "536870911\n")
  ((fx- 536870911 1) => "536870910\n")
  ((fx- -536870911 1) => "-536870912\n")
  ((fx- -536870912 -1) => "-536870911\n")
  ((fx- 1 536870911) => "-536870910\n")
  ((fx- -1 536870911) => "-536870912\n")
  ((fx- 1 -536870910) => "536870911\n")
  ((fx- -1 -536870912) => "536870911\n")
  ((fx- 536870911 536870911) => "0\n")
  ;((fx- 536870911 -536870912) => "-1\n")
  ((fx- -536870911 -536870912) => "1\n")
  ((fx- 1 (fx- 2 3)) => "2\n")
  ((fx- 1 (fx- 2 -3)) => "-4\n")
  ((fx- 1 (fx- -2 3)) => "6\n")
  ((fx- 1 (fx- -2 -3)) => "0\n")
  ((fx- -1 (fx- 2 3)) => "0\n")
  ((fx- -1 (fx- 2 -3)) => "-6\n")
  ((fx- -1 (fx- -2 3)) => "4\n")
  ((fx- -1 (fx- -2 -3)) => "-2\n")
  ((fx- 0 (fx- -2 -3)) => "-1\n")
  ((fx- (fx- 1 2) 3) => "-4\n")
  ((fx- (fx- 1 2) -3) => "2\n")
  ((fx- (fx- 1 -2) 3) => "0\n")
  ((fx- (fx- 1 -2) -3) => "6\n")
  ((fx- (fx- -1 2) 3) => "-6\n")
  ((fx- (fx- -1 2) -3) => "0\n")
  ((fx- (fx- -1 -2) 3) => "-2\n")
  ((fx- (fx- -1 -2) -3) => "4\n")
  ((fx- (fx- (fx- (fx- (fx- (fx- (fx- (fx- 1 2) 3) 4) 5) 6) 7) 8) 9) => "-43\n")
  ((fx- 1 (fx- 2 (fx- 3 (fx- 4 (fx- 5 (fx- 6 (fx- 7 (fx- 8 9)))))))) => "5\n")
)

(add-tests-with-string-output "fx*"
  ((fx* 2 3) => "6\n")
  ((fx* 2 -3) => "-6\n")
  ((fx* -2 3) => "-6\n")
  ((fx* -2 -3) => "6\n")
  ((fx* 536870911 1) => "536870911\n")
  ((fx* 536870911 -1) => "-536870911\n")
  ((fx* -536870912 1) => "-536870912\n")
  ((fx* -536870911 -1) => "536870911\n")
  ((fx* 2 (fx* 3 4)) => "24\n")
  ((fx* (fx* 2 3) 4) => "24\n")
  ((fx* (fx* (fx* (fx* (fx* 2 3) 4) 5) 6) 7) => "5040\n")
  ((fx* 2 (fx* 3 (fx* 4 (fx* 5 (fx* 6 7))))) => "5040\n")
)

(add-tests-with-string-output "fxlogand and fxlogor"
  ((fxlogor 3 16) => "19\n")
  ((fxlogor 3 5)  => "7\n")
  ((fxlogor 3 7)  => "7\n")
  ((fxlognot (fxlogor (fxlognot 7) 1)) => "6\n")
  ((fxlognot (fxlogor 1 (fxlognot 7))) => "6\n")
  ((fxlogand 3 7) => "3\n")
  ((fxlogand 3 5) => "1\n")
  ((fxlogand 2346 (fxlognot 2346)) => "0\n")
  ((fxlogand (fxlognot 2346) 2346) => "0\n")
  ((fxlogand 2376 2376) => "2376\n")
)

(add-tests-with-string-output "fx="
  ((fx= 12 13) => "#f\n")
  ((fx= 12 12) => "#t\n")
  ((fx= 16 (fx+ 13 3)) => "#t\n")
  ((fx= 16 (fx+ 13 13)) => "#f\n")
  ((fx= (fx+ 13 3) 16) => "#t\n")
  ((fx= (fx+ 13 13) 16) => "#f\n")
)

(add-tests-with-string-output "fx<"
  ((fx< 12 13) => "#t\n")
  ((fx< 12 12) => "#f\n")
  ((fx< 13 12) => "#f\n")
  ((fx< 16 (fx+ 13 1)) => "#f\n")
  ((fx< 16 (fx+ 13 3)) => "#f\n")
  ((fx< 16 (fx+ 13 13)) => "#t\n")
  ((fx< (fx+ 13 1) 16) => "#t\n")
  ((fx< (fx+ 13 3) 16) => "#f\n")
  ((fx< (fx+ 13 13) 16) => "#f\n")
)

(add-tests-with-string-output "fx<="
  ((fx<= 12 13) => "#t\n")
  ((fx<= 12 12) => "#t\n")
  ((fx<= 13 12) => "#f\n")
  ((fx<= 16 (fx+ 13 1)) => "#f\n")
  ((fx<= 16 (fx+ 13 3)) => "#t\n")
  ((fx<= 16 (fx+ 13 13)) => "#t\n")
  ((fx<= (fx+ 13 1) 16) => "#t\n")
  ((fx<= (fx+ 13 3) 16) => "#t\n")
  ((fx<= (fx+ 13 13) 16) => "#f\n")
)

(add-tests-with-string-output "fx>"
  ((fx> 12 13) => "#f\n")
  ((fx> 12 12) => "#f\n")
  ((fx> 13 12) => "#t\n")
  ((fx> 16 (fx+ 13 1)) => "#t\n")
  ((fx> 16 (fx+ 13 3)) => "#f\n")
  ((fx> 16 (fx+ 13 13)) => "#f\n")
  ((fx> (fx+ 13 1) 16) => "#f\n")
  ((fx> (fx+ 13 3) 16) => "#f\n")
  ((fx> (fx+ 13 13) 16) => "#t\n")
)

(add-tests-with-string-output "fx>="
  ((fx>= 12 13) => "#f\n")
  ((fx>= 12 12) => "#t\n")
  ((fx>= 13 12) => "#t\n")
  ((fx>= 16 (fx+ 13 1)) => "#t\n")
  ((fx>= 16 (fx+ 13 3)) => "#t\n")
  ((fx>= 16 (fx+ 13 13)) => "#f\n")
  ((fx>= (fx+ 13 1) 16) => "#f\n")
  ((fx>= (fx+ 13 3) 16) => "#t\n")
  ((fx>= (fx+ 13 13) 16) => "#t\n")
)

(add-tests-with-string-output "if"
  ((if (fx= 12 13) 12 13) => "13\n")
  ((if (fx= 12 12) 13 14) => "13\n")
  ((if (fx< 12 13) 12 13) => "12\n")
  ((if (fx< 12 12) 13 14) => "14\n")
  ((if (fx< 13 12) 13 14) => "14\n")
  ((if (fx<= 12 13) 12 13) => "12\n")
  ((if (fx<= 12 12) 12 13) => "12\n")
  ((if (fx<= 13 12) 13 14) => "14\n")
  ((if (fx> 12 13) 12 13) => "13\n")
  ((if (fx> 12 12) 12 13) => "13\n")
  ((if (fx> 13 12) 13 14) => "13\n")
  ((if (fx>= 12 13) 12 13) => "13\n")
  ((if (fx>= 12 12) 12 13) => "12\n")
  ((if (fx>= 13 12) 13 14) => "13\n")
)
