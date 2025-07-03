open Ast (*contents in ast file required*)

type value=
|VInt of int
|VBool of bool
|VClosure of string*expr*env and
env=(string*value)list



let rec eval(e:expr)(env:env):value =
match e with
 Int n->VInt n|
 
 Bool x->VBool x|

 Var x->(try List.assoc x env with Not_found->failwith("Unbound Variabe: "^x))|

Fun(param,body)->VClosure(param,body,env)|

App(e1,e2)->let v1=eval e1 env in
let v2=eval e2 env in(
  match v1 with
  VClosure(param,body,cl_env)->
    eval body((param,v2)::cl_env)|
    _->failwith " Expected a function"
)|
Let (x,e1,e2)->let v1=eval e1 env in
eval e2 ((x,v1)::env)|

LetRec(f,Fun(param,body),e2)->
  let rec_env=ref[] in
  let closure=VClosure(param,body,(f,VClosure(param,body,!rec_env))::env)in
  rec_env:=(f,closure)::env;
  eval e2 !rec_env|

  LetRec(_,_,_)->failwith "LetRec must bind with a function"|

If(e1,e2,e3)->
  let v1=eval e1 env in
  (match v1 with
  VBool true->eval e2 env|
  VBool false->eval e3 env|
  _->failwith "Expecting a boolean for e1")






