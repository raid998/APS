assocArg([],List,List).
assocArg([arg(id(X),Y)|Tail],List,[(X,Y)|Rest]) :- assocArg(Tail,List,Rest).

assoc(X, [(X,V)|_], V).
assoc(X, [_|XS], V) :- assoc(X, XS, V).

initCtx([(true,bool),(false,bool),(not,types([bool],bool)),(eq,types([int,int],bool)),(lt,types([int,int],bool)),(add,types([int,int],int)),(sub,types([int,int],int)),(mul,types([int,int],int)),(div,types([int,int],int))]).

typeCheck(G,[E],[T]) :- typeExpr(G,E,T).
typeCheck(G,[E|ES],[T|TS]) :- typeExpr(G,E,T),typeCheck(G,ES,TS).

argCheck(G,[arg(X,Y)|_]) :- typeExpr(G,X,Y). 
argCheck(G,[_|T]) :- argCheck(G,T).

genTypes([],TS,TS).
genTypes([arg(_,T)|ARGS],TS,[T|REST]) :- genTypes(ARGS,TS,REST).

/* SUITE DE COMMANDES */
typeSeq(G,[X],void) :- typeEcho(G,X,void).
typeSeq(G,[D|X],void) :- typeDef(G,D,G1),typeSeq(G1,X,void).


/* PROG*/

typeProg(prog(P),void) :- initCtx(G0),typeSeq(G0,P,void).

/* DEFS */

typeDef(G,const(id(X),T,E),[(X,T)|G]) :- typeExpr(G,E,T).
typeDef(G,funDef(id(X),T,args(A),E),[(X,types(TS,T))|G]) :- assocArg(A,G,G1), typeExpr(G1,E,T),genTypes(A,[],TS).
typeDef(G,funRecDef(id(X),T,args(A),E),[(X,types(TS,T))|G]) :- assocArg(A,G,G1), genTypes(A,[],TS), typeExpr([(x,types(TS,T))|G1],E,T).


/* ECHO  */

typeEcho(G,echo(E),void) :- typeExpr(G,E,int).

/* ENTIER */
typeExpr(_,num(_),int).

typeExpr(G,id(X),T):- assoc(X,G,T).

typeExpr(G,if(C,B,A),T) :- typeExpr(G,C,bool),typeExpr(G,B,T),typeExpr(G,A,T).

typeExpr(G,and(A,B),bool) :- typeExpr(G,A,bool), typeExpr(G,B,bool).

typeExpr(G,or(A,B),bool) :- typeExpr(G,A,bool), typeExpr(G,B,bool).

typeExpr(G,app(E,ES),T) :- typeExpr(G,E,types(TA,T)),typeCheck(G,ES,TA).

typeExpr(G,fun(args(A),E),types(TS,T)) :- assocArg(A,G,NewG),typeExpr(NewG,E,T),genTypes(A,[],TS).


type(E,T) :- initCtx(G0), typeExpr(G0,E,T).

main_stdin :-
	read(user_input,T),
	typeProg(T,R),
	print(R).