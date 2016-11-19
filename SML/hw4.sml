fun dict_push ((a,b) : string*string, (l1,l2)::l : (string*string) list) = if a = l1 then
(a,b)::l
else
[(l1,l2)] @ dict_push ((a,b),l)
|dict_push ((a,b),[]) = (a,b)::[]

fun find (m : string, (a,b)::l : (string*string) list) = if a=m then
                                                true 
                                                else
                                                  find (m,l)
| find (m,[]) = false;

fun dict_pop (m : string, (a,b)::l : (string*string) list) = if a=m then
                                                 b
                                                else
                                                  dict_pop (m,l)
| dict_pop (m,[]) = ":error:";

val dict = [("z","z")]

fun push item (m : string list)         =  (item::m)
  fun pop ( first_item::m : string list) = (first_item, m)
fun isEmpty (m :string list)=
if m=[] then
 true
 else
  false
val v = []
val v = push "\n" v
val v = push "z" v
val v = push "\n" v
val v = push "z" v
val v = push "\n" v
val v = push "z" v

fun is_Digit (n : string)=
let
 fun isDigit (n : string)=
let
  val num_int = valOf(Int.fromString(n))
  val check_real = valOf( Real.fromString(n))
  val num_real = Real.fromInt(num_int)
  
 in
   ( if Real.==(check_real,num_real) then
    true
 else
  false)
end
in
  if n = ":unit:" orelse n = ":true:" orelse n = ":false:" orelse n = ":error:" orelse Char.isAlpha(String.sub(n,0))  then
   false
  else
   isDigit(n)
 end  

  
  fun push_call (c) = String.isSubstring "push" c;
  fun pop_call (c) = String.isSubstring "pop" c;
 fun let_call (c) = String.isSubstring "let" c;
 fun is_End (c) = String.isSubstring "end" c;
  fun add_call (c) = String.isSubstring "add" c;
  fun sub_call (c) = String.isSubstring "sub" c;
  fun is_Swap (c) = String.isSubstring "swap" c;
  fun mul_call (c) = String.isSubstring "mul" c;
  fun div_call (c) = String.isSubstring "div" c;
  fun rem_call (c) = String.isSubstring "rem" c;
  fun and_call (c) = String.isSubstring "and" c;
   fun bind_call (c) = String.isSubstring "bind" c;
   fun less_call (c) = String.isSubstring "lessThan" c;
   fun equal_call (c) = String.isSubstring "equal" c;
   fun not_call (c) = String.isSubstring "not" c;
   fun or_call (c) = String.isSubstring "or" c;
  fun quit_call (c) = String.isSubstring "quit" c;
  fun is_True (c) = String.isSubstring ":true:" c;
  fun if_call (c) = String.isSubstring "if" c;
  fun is_False (c) = String.isSubstring ":false:" c;
  fun error_call (c) = String.isSubstring ":error:" c;
   fun neg_call (c) = String.isSubstring "neg" c;

fun push1(no:string,v : string list,dict :(string*string)list)=
     let
       val v = push "\n" v
       val v = push no v

    in
       ("",v,dict)
     end

fun Error ( v : string list,dict : (string*string)list) =
let
 val v = push "\n" v
  val v =push ":error:" v
  
 in
  ("",v,dict)
end

fun Bind (v : string list,dict : (string*string)list) =
let
val a = List.nth(v,0)

val b = List.nth(v,2)

val a1 = find (a,dict)
val b1 = find (b,dict)
val ag = dict_pop(a,dict)
fun Bind1 (v : string list,n : int,dict : (string*string)list) =
let
val (a,v) = pop v
val (blank,v) = pop v
val (b,v) = pop v
val ag = dict_pop(a,dict)
val c = ":unit:"
val v = push c v
val dict =
if n=1 then 
 dict_push ((b,ag),dict)
else
 dict_push ((b,a),dict)
in
("",v,dict)
end
 in
   if (String.compare(a,"z") = EQUAL) orelse (String.compare(b,"z") = EQUAL) orelse (String.compare(a,":error:") = EQUAL) then
   Error (v,dict)
   else if (b1=false) then
    Error (v,dict) 
   else if (a1=true) andalso (ag="empty") then
   Error (v,dict)
   else if (a1=true) andalso (ag<>"empty") then
    Bind1(v,1,dict)
   else
    Bind1 (v,0,dict)

end

fun str_push(no : string,v :string list, dict : (string*string)list)=
 let 
  val size = String.size(no)
  val str = String.substring(no,0,size+(~1))
in
 push1(no,v,dict)
end

fun var_push(no : string,v :string list, dict : (string*string)list)=
 let
  val dict = dict_push((no,"empty"),dict)
in
 push1(no,v,dict)
end 

fun neg_push(no : string,v :string list,dict : (string*string)list)=
 let
  val size = String.size(no)
  val str = String.substring(no,1,size+(~1))
   val num = ~(valOf(Int.fromString(str)))
  val final = Int.toString(num)
in
 push1(final,v,dict)
end
fun push0 (c : string, v : string list,dict : (string*string)list) =
let

  val b = String.size(c) + (~6)
  val d = b + (~1) 
val no = String.substring(c,5,b)
 val size = String.size(no)
val first = String.sub(c,5)
val nocheck = find (no,dict)
val second = if size<2 then
              #"0" 
               else
              String.sub(no,1)

              
  in 
     if ord(first)= 45 then
      neg_push (no,v,dict)
     else if (Char.isAlpha(first)<>true) andalso (Char.isAlpha(second) = true) then
    str_push (no,v,dict)
   else if Char.isAlpha(first) andalso (nocheck = false)  then
     var_push (no,v,dict)
  else if Char.isAlpha(first) andalso (nocheck <> false)  then
     push1 (no,v,dict)
  else if is_Digit(no) then
     push1 (no,v,dict) 
   else
    Error (v,dict)
end

fun pop0 (v : string list,dict : (string*string)list) =
let
val a = List.nth(v,0)


fun pop1 (v : string list,dict : (string*string)list) =
let
val (item,v) = pop v
val (item,v) = pop v
 in
  ("",v,dict)
end

 in
  if (String.compare(a,"z") = EQUAL)    then
   Error (v,dict)
   
   else
    pop1(v,dict)
   
end

fun If (v : string list,dict : (string*string)list) =
let
val a = List.nth(v,0)
val b = List.nth(v,2)
val c = List.nth(v,4)
val cget = dict_pop(c,dict)

fun If1 (v : string list,dict : (string*string)list) =
let
val (a,v) = pop v
val (blank,v) = pop v
val (b,v) = pop v
val (blank,v) = pop v
val (c,v) = pop v
val cget = dict_pop(c,dict)
val d = if (String.compare(c,":true:")=EQUAL) orelse (String.compare(cget,":true:")=EQUAL) then
          a
         else
          b
val v = push d v
in
("",v,dict)
end
 in
  if ((String.compare(a,"z") = EQUAL) orelse (String.compare(b,"z") = EQUAL) orelse (String.compare(c,"z")=EQUAL)) then
   Error (v,dict)
 else if ((String.compare(c,":true:")=EQUAL) orelse (String.compare(c,":false:")=EQUAL) orelse (String.compare(cget,":true:")=EQUAL) orelse (String.compare(cget,":false:")=EQUAL)) then
     If1(v,dict)
     else
      Error(v,dict)

end

fun Or (v : string list,dict : (string*string)list) =
let
val a = List.nth(v,0)

val b = List.nth(v,2)
val aget = dict_pop(a,dict)
val bget = dict_pop(b,dict)

fun Or1 (v : string list,m : string,dict : (string*string)list) =
let
val (a,v) = pop v
val (blank,v) = pop v
val (b,v) = pop v
val av = find (a,dict)
val bv = find (a,dict)
val aget = dict_pop(a,dict)
val bget = dict_pop(b,dict)
val v = push m v
in
("",v,dict)

end
 in
  if ((String.compare(a,"z") = EQUAL) orelse (String.compare(b,"z") = EQUAL))  then
   Error (v,dict)
   else if ((String.compare(a,":true:") = EQUAL ) orelse (String.compare(aget,":true:") = EQUAL )) andalso ((String.compare(b,":true:") = EQUAL) orelse (String.compare(bget,":true:") = EQUAL))   then
  Or1 (v,":true:",dict)
  
   else if ((String.compare(a,":true:") = EQUAL) orelse (String.compare(aget,":true:") = EQUAL) ) andalso ((String.compare(b,":false:") = EQUAL) orelse (String.compare(bget,":false:") = EQUAL))  then
   Or1 (v,":true:",dict)
   
   
   else if ((String.compare(a,":false:") = EQUAL) orelse (String.compare(aget,":false:") = EQUAL)) andalso ((String.compare(b,":true:") = EQUAL) orelse (String.compare(bget,":true:") = EQUAL))  then
   Or1 (v,":true:",dict)
   else if ((String.compare(a,":false:") = EQUAL) orelse (String.compare(aget,":false:") = EQUAL)) andalso ((String.compare(b,":false:") = EQUAL) orelse (String.compare(bget,":false:") = EQUAL))  then
   Or1 (v,":false:",dict)
   else
    Error (v,dict)

end

fun And (v : string list,dict : (string*string)list) =
let
val a = List.nth(v,0)

val b = List.nth(v,2)
val aget = dict_pop(a,dict)
val bget = dict_pop(b,dict)

fun And1 (v : string list,m : string,dict : (string*string)list) =
let
val (a,v) = pop v
val (blank,v) = pop v
val (b,v) = pop v

val v = push m v
in
("",v,dict)
         
end       
 in      
  if ((String.compare(a,"z") = EQUAL) orelse (String.compare(b,"z") = EQUAL))  then
   Error (v,dict)
   else if (((String.compare(a,":true:") = EQUAL ) orelse (String.compare(aget,":true:") = EQUAL ) ) andalso ((String.compare(b,":true:") = EQUAL) orelse (String.compare(bget,":true:") = EQUAL)))   then
  And1 (v,":true:",dict) 
   else if (((String.compare(a,":true:") = EQUAL) orelse (String.compare(aget,":true:") = EQUAL))  andalso ((String.compare(b,":false:") = EQUAL) orelse (String.compare(bget,":false:") = EQUAL)))  then
   And1 (v,":false:",dict) 
   else if (((String.compare(b,":true:") = EQUAL) orelse (String.compare(bget,":true:") = EQUAL))  andalso ((String.compare(a,":false:") = EQUAL) orelse (String.compare(aget,":false:") = EQUAL)))  then
   And1 (v,":false:",dict) 
   
   else if (((String.compare(a,":false:") = EQUAL) orelse (String.compare(aget,":false:") = EQUAL)) andalso ((String.compare(b,":true:") = EQUAL) orelse (String.compare(bget,":true:") = EQUAL)))  then
   And1 (v,":false:",dict)
   
   else if (((String.compare(b,":false:") = EQUAL) orelse (String.compare(bget,":false:") = EQUAL)) andalso ((String.compare(a,":true:") = EQUAL) orelse (String.compare(aget,":true:") = EQUAL)))  then
   And1 (v,":false:",dict)
   
   else if (((String.compare(a,":false:") = EQUAL) orelse (String.compare(aget,":false:") = EQUAL) )andalso ((String.compare(b,":false:") = EQUAL) orelse (String.compare(bget,":false:") = EQUAL)))  then
   And1 (v,":false:",dict)
   else
    Error (v,dict)

end

fun Add (v : string list,dict : (string*string)list) =
let
val a = List.nth(v,0)

val b = List.nth(v,2)


fun Add1 (v : string list,dict : (string*string)list) =
let
val (a,v) = pop v
val (blank,v) = pop v
val (b,v) = pop v
val av = find (a,dict)
val bv = find (a,dict)
val aget=dict_pop(a,dict)
val bget=dict_pop(b,dict)
val a1 = if is_Digit(a) then
           valOf(Int.fromString(a))
          else if (av=true) andalso (aget<>"empty") then
          valOf(Int.fromString(dict_pop(a,dict)))
          else
           99999 
val b1 = if is_Digit(b) then
           valOf(Int.fromString(b)) 
           else if (bv=true) andalso (bget<>"empty") then
          valOf(Int.fromString(dict_pop(b,dict)))
          else
           99999 
val c1 = if (a1= 99999) orelse (b1= 99999) then
         ":error:"^"\n"^a^"\n"^b
        else  
        Int.toString(a1+b1)
val v = push c1 v
in
("",v,dict)
end
 in
  if (String.compare(a,"z") = EQUAL) orelse (String.compare(b,"z") = EQUAL) then
   Error (v,dict) 
   else if (is_Digit (a) orelse Char.isAlpha(String.sub(a,0))) andalso (is_Digit (b) orelse Char.isAlpha(String.sub(b,0))) then
   Add1 (v,dict)
  else   
    Error (v,dict)
          
end 
 
fun Not (v : string list,dict : (string*string)list) =
let
val a = List.nth(v,0)
val aget = dict_pop(a,dict)

fun Not1 (v : string list,m : string,dict : (string*string)list) =
let
val (a,v) = pop v
val v = push m v
in
("",v,dict)

end
 in
  if (String.compare(a,"z") = EQUAL)  then
   Error (v,dict)
   else if ((String.compare(a,":true:") = EQUAL) orelse (String.compare(aget,":true:") = EQUAL ))    then
   Not1 (v,":false:",dict)
   else if ((String.compare(a,":false:") = EQUAL) orelse (String.compare(aget,":false:") = EQUAL ) )  then
   Not1 (v,":true:",dict)
   else
    Error (v,dict)

end
 
fun Equal (v : string list,dict : (string*string)list) =
let
val a = List.nth(v,0)

val b = List.nth(v,2)


fun Equal1 (v : string list,dict : (string*string)list) =
let
val (a,v) = pop v
val (blank,v) = pop v
val (b,v) = pop v
val av = find (a,dict)
val bv = find (a,dict)
val aget=dict_pop(a,dict)
val bget=dict_pop(b,dict)
val a1 = if is_Digit(a) then
           valOf(Int.fromString(a))
          else if (av=true) andalso (aget<>"empty") then
          valOf(Int.fromString(dict_pop(a,dict)))
          else
           99999
val b1 = if is_Digit(b) then
           valOf(Int.fromString(b))
           else if (bv=true) andalso (bget<>"empty") then
          valOf(Int.fromString(dict_pop(b,dict)))
          else
           99999
val c1 = if (a1= 99999) orelse (b1= 99999) then
         ":error:"^"\n"^a^"\n"^b
        else if (a1 = b1) then
         ":true:"
            else
            ":false:"
        
val v = push c1 v
in
("",v,dict)
end
 in
  if (String.compare(a,"z") = EQUAL) orelse (String.compare(b,"z") = EQUAL) then
   Error (v,dict)
   else if (is_Digit (a) orelse Char.isAlpha(String.sub(a,0))) andalso (is_Digit (b) orelse Char.isAlpha(String.sub(b,0))) then
   Equal1 (v,dict)
  else
    Error (v,dict)

end

fun Less (v : string list,dict : (string*string)list) =
let
val a = List.nth(v,0)

val b = List.nth(v,2)


fun Less1 (v : string list,dict : (string*string)list) =
let
val (a,v) = pop v
val (blank,v) = pop v
val (b,v) = pop v
val av = find (a,dict)
val bv = find (a,dict)
val aget=dict_pop(a,dict)
val bget=dict_pop(b,dict)
val a1 = if is_Digit(a) then
           valOf(Int.fromString(a))
          else if (av=true) andalso (aget<>"empty") then
          valOf(Int.fromString(dict_pop(a,dict)))
          else
           99999
val b1 = if is_Digit(b) then
           valOf(Int.fromString(b))
           else if (bv=true) andalso (bget<>"empty") then
          valOf(Int.fromString(dict_pop(b,dict)))
          else
           99999
val c1 = if (a1= 99999) orelse (b1= 99999) then
         ":error:"^"\n"^a^"\n"^b
        else if (a1 > b1) then
         ":true:"
            else
            ":false:"
        
val v = push c1 v
in
("",v,dict)
end
 in
  if (String.compare(a,"z") = EQUAL) orelse (String.compare(b,"z") = EQUAL) then
   Error (v,dict)
   else if (is_Digit (a) orelse Char.isAlpha(String.sub(a,0))) andalso (is_Digit (b) orelse Char.isAlpha(String.sub(b,0))) then
   Less1 (v,dict)
  else
    Error (v,dict)

end



fun Sub (v : string list,dict : (string*string)list) =
let
val a = List.nth(v,0)

val b = List.nth(v,2)


fun Sub1 (v : string list,dict : (string*string)list) =
let
val (a,v) = pop v
val (blank,v) = pop v
val (b,v) = pop v
val av = find (a,dict)
val bv = find (a,dict)
val aget=dict_pop(a,dict)
val bget=dict_pop(b,dict)
val a1 = if is_Digit(a) then
           valOf(Int.fromString(a))
          else if (av=true) andalso (aget<>"empty") then
          valOf(Int.fromString(dict_pop(a,dict)))
          else
           99999
val b1 = if is_Digit(b) then
           valOf(Int.fromString(b))
           else if (bv=true) andalso (bget<>"empty") then
          valOf(Int.fromString(dict_pop(b,dict)))
          else
           99999
val c1 = if (a1= 99999) orelse (b1= 99999) then
         ":error:"^"\n"^a^"\n"^b
        else
        Int.toString(b1-a1)
val v = push c1 v
in
("",v,dict)
end
 in
  if (String.compare(a,"z") = EQUAL) orelse (String.compare(b,"z") = EQUAL) then
   Error (v,dict)
   else if (is_Digit (a) orelse Char.isAlpha(String.sub(a,0))) andalso (is_Digit (b) orelse Char.isAlpha(String.sub(b,0))) then
   Sub1 (v,dict)
  else
    Error (v,dict)

end

fun Mul (v : string list,dict : (string*string)list) =
let
val a = List.nth(v,0)

val b = List.nth(v,2)


fun Mul1 (v : string list,dict : (string*string)list) =
let
val (a,v) = pop v
val (blank,v) = pop v
val (b,v) = pop v
val av = find (a,dict)
val bv = find (a,dict)
val aget=dict_pop(a,dict)
val bget=dict_pop(b,dict)
val a1 = if is_Digit(a) then
           valOf(Int.fromString(a))
          else if (av=true) andalso (aget<>"empty") then
          valOf(Int.fromString(dict_pop(a,dict)))
          else
           99999
val b1 = if is_Digit(b) then
           valOf(Int.fromString(b))
           else if (bv=true) andalso (bget<>"empty") then
          valOf(Int.fromString(dict_pop(b,dict)))
          else
           99999
val c1 = if (a1= 99999) orelse (b1= 99999) then
         ":error:"^"\n"^a^"\n"^b 
        else
        Int.toString(b1*a1)
val v = push c1 v
in
("",v,dict)
end
 in
  if (String.compare(a,"z") = EQUAL) orelse (String.compare(b,"z") = EQUAL) then
   Error (v,dict)
   else if (is_Digit (a) orelse Char.isAlpha(String.sub(a,0))) andalso (is_Digit (b) orelse Char.isAlpha(String.sub(b,0))) then
   Mul1 (v,dict)
  else
    Error (v,dict)

end

fun Div (v : string list,dict : (string*string)list) =
let
val a = List.nth(v,0)

val b = List.nth(v,2)


fun Div1 (v : string list,dict : (string*string)list) =
let
val (a,v) = pop v
val (blank,v) = pop v
val (b,v) = pop v
val av = find (a,dict)
val bv = find (a,dict)
val aget=dict_pop(a,dict)
val bget=dict_pop(b,dict)
val a1 = if is_Digit(a) then
           valOf(Int.fromString(a))
          else if (av=true) andalso (aget<>"empty") then
          valOf(Int.fromString(dict_pop(a,dict)))
          else
           99999
val b1 = if is_Digit(b) then
           valOf(Int.fromString(b))
           else if (bv=true) andalso (bget<>"empty") then
          valOf(Int.fromString(dict_pop(b,dict)))
          else
           99999
val c1 = if (a1= 99999) orelse (b1= 99999) then
         ":error:"^"\n"^a^"\n"^b
        else
        Int.toString(b1 div a1)
val v = push c1 v
in
("",v,dict)
end
 in
  if (String.compare(a,"z") = EQUAL) orelse (String.compare(b,"z") = EQUAL) then
   Error (v,dict)
   else if (is_Digit (a) orelse Char.isAlpha(String.sub(a,0))) andalso (is_Digit (b) orelse Char.isAlpha(String.sub(b,0))) then
   Div1 (v,dict)
  else
    Error (v,dict)

end

fun rem_0 (v : string list,dict : (string*string)list) =
let
val a = List.nth(v,0)

val b = List.nth(v,2)


fun rem_1 (v : string list,dict : (string*string)list) =
let
val (a,v) = pop v
val (blank,v) = pop v
val (b,v) = pop v
val av = find (a,dict)
val bv = find (a,dict)
val aget=dict_pop(a,dict)
val bget=dict_pop(b,dict)
val a1 = if is_Digit(a) then
           valOf(Int.fromString(a))
          else if (av=true) andalso (aget<>"empty") then
          valOf(Int.fromString(dict_pop(a,dict)))
          else
           99999
val b1 = if is_Digit(b) then
           valOf(Int.fromString(b))
           else if (bv=true) andalso (bget<>"empty") then
          valOf(Int.fromString(dict_pop(b,dict)))
          else
           99999
val c1 = if (a1= 99999) orelse (b1= 99999) then
         ":error:"^"\n"^a^"\n"^b
        else
        Int.toString(b1 mod a1)
val v = push c1 v
in
("",v,dict)
end
 in
  if (String.compare(a,"z") = EQUAL) orelse (String.compare(b,"z") = EQUAL) then
   Error (v,dict)
   else if (is_Digit (a) orelse Char.isAlpha(String.sub(a,0))) andalso (is_Digit (b) orelse Char.isAlpha(String.sub(b,0))) then
   rem_1 (v,dict)
  else
    Error (v,dict)

end

fun swap0 (v : string list,dict : (string*string)list) =
let
val a = List.nth(v,0)

val b = List.nth(v,2)


fun swap1 (v : string list,dict : (string*string)list) =
let 
val (a,v) = pop v
val (blank,v) = pop v
val (b,v) = pop v
val v = push a v
val v = push "\n" v
val v = push b v
in
("",v,dict)
end
 in
  if ((String.compare(a,"z") = EQUAL) andalso (String.compare(b,"z") = EQUAL))  then
   Error (v,dict)
   else if (String.compare(a,"z") <> EQUAL) andalso (String.compare(b,"z") = EQUAL)  then
   Error (v,dict)
   else
    swap1(v,dict)
   
end


fun Neg (v : string list,dict : (string*string)list) =
let
val a = List.nth(v,0)

fun Neg1 (v : string list,dict : (string*string)list) =
let
val (a,v) = pop v
val av = find (a,dict)

val aget=dict_pop(a,dict)

val a1 = if is_Digit(a) then
           valOf(Int.fromString(a))
          else if (av=true) andalso (aget<>"empty") then
          valOf(Int.fromString(dict_pop(a,dict)))
          else
           99999

val c1 = if (a1= 99999) then
         ":error:"^"\n"^a
        else
        Int.toString(~a1)
val v = push c1 v
in
("",v,dict)
end
 in
  if (String.compare(a,"z") = EQUAL) then
   Error (v,dict)
   else if is_Digit (a) orelse Char.isAlpha(String.sub(a,0))  then
   Neg1 (v,dict)
  else
    Error (v,dict)

end
fun stringneg [] = []
|stringneg(h::t)=
           if String.sub(h,0) = #"~" then
 let
 val c = String.substring(h,1,String.size(h)+(~1))
 val d = "-"^c
in
 [d] @ stringneg(t)
end
else
            [h] @ stringneg(t);

fun stringquote [] = []
|stringquote(h::t)=  if String.isSubstring"\"" h then
 [String.substring(h,1,String.size(h)+(~2))] @ stringquote(t)
else  
            [h] @ stringquote(t); 

fun Quit (v : string list,dict : (string*string)list) =
 let
 val v = stringneg v
 val v = stringquote v 
 val final = concat v 
  val b = String.size(final) + (~6)
val c = String.substring(final,0,b)
   
 in
    (c,v,dict)
end




fun Let ( v : string list,dict : (string*string)list) =
let
 val v = push "\n" v 
 val v =push "py" v
 in
  ("",v,dict)
end

fun End (v : string list, dict : (string*string)list)=
let
val (a,v) = pop v
val (blank,v) = pop v


fun End1 (v : string list , dict : (string*string)list,a : string)=
 let
    val (b,v) = pop v
    val (blank,v) = pop v
 in
   if b <> "py" then
    End1(v,dict,a)
    else
     push1(a,v,dict)
  end

in
    End1(v,dict,a)
end  

fun T (c : string, v : string list,dict : (string*string)list) =
let
 val b = String.size(c) + (~1)
val c = String.substring(c,0,b)
 val v = push "\n" v
  val v =push c v
 in
  ("",v,dict)
end

fun F (c : string, v : string list,dict : (string*string)list) =
let
 val b = String.size(c) + (~1)
val c = String.substring(c,0,b)
val v = push "\n" v
  val v =push c v
 in
  ("",v,dict)
end


fun main (line : string, v : string list,dict : (string*string)list) =

   if push_call (line) then
     push0 (line,v,dict)
    else if pop_call line then
     pop0 (v,dict)
     else if add_call line then
     Add (v,dict)
     else if sub_call line then
     Sub (v,dict)
     else if div_call line then
     Div (v,dict)
     else if mul_call line then
     Mul (v,dict)
     else if rem_call line then
     rem_0 (v,dict)
    else if neg_call line then
     Neg (v,dict) 
    else if or_call line then
     Or (v,dict)
    else if bind_call line then
     Bind (v,dict)
     else if and_call line then
     And (v,dict)
     else if equal_call line then
      Equal (v,dict)
      else if less_call line then
     Less (v,dict)
     else if not_call line then
     Not (v,dict)
      else if if_call line then
     If (v,dict)
     else if quit_call line then
      Quit(v,dict)
     else if error_call line then
     Error (v,dict)
     else if let_call line then
     Let (v,dict)
     else if line ="" then
      ("",v,dict)
      else if is_Swap line then
      swap0(v,dict)
      else if is_End line then
       End(v,dict)
      
     else if is_True line then
      T (line,v,dict)
      else
       F (line,v,dict)
      

fun hw4(inputFile : string, outputFile : string) =
let
        val inStream = TextIO.openIn inputFile
        val outStream = TextIO.openOut outputFile
        val readLine = TextIO.inputLine inStream
       

        fun helper(readLine : string option , v :string list,dict : (string*string)list) =
        case readLine of
         NONE=>(TextIO.closeIn inStream; TextIO.closeOut outStream)
         |SOME(a)=>
         (let
          
         
         val (item,v,dict) = main(valOf(readLine),v,dict)
         
         in
                                (TextIO.output(outStream,item);helper(TextIO.inputLine inStream,v,dict))                     
          end)


in
        helper(readLine,v,dict)
end


