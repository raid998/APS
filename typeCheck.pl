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


/** entier **/
typeExpr(_,num(_),int).

typeExpr(G,id(X),T):- assoc(X,G,T).

typeExpr(G,if(C,B,A),T) :- typeExpr(G,C,bool),typeExpr(G,B,T),typeExpr(G,A,T).

typeExpr(G,and(A,B),bool) :- typeExpr(G,A,bool), typeExpr(G,B,bool).

typeExpr(G,or(A,B),bool) :- typeExpr(G,A,bool), typeExpr(G,B,bool).

typeExpr(G,app(E,ES),T) :- typeExpr(G,E,types(TA,T)),typeCheck(G,ES,TA).

typeExpr(G,fun(args(A),E),types(TS,T)) :- assocArg(A,G,NewG),typeExpr(NewG,E,T),genTypes(A,[],TS).

type(E,T) :- initCtx(G0), typeExpr(G0,E,T).

