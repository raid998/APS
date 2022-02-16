assoc(X, [(X,V)|_], V).
assoc(X, [_|XS], V) :- assoc(X, XS, V).

initCtx([(true,bool),(false,bool)]).

/** entier **/
typeExpr(_,num(_),int).
typeExpr(G,id(X),T):- assoc(X,G,T).
typeExpr(G,if(C,B,A),T) :- typeExpr(G,C,bool),typeExpr(G,B,T),typeExpr(G,A,T).
typeExpr(G,and(A,B),bool) :- typeExpr(G,A,bool), typeExpr(G,B,bool).
typeExpr(G,or(A,B),bool) :- typeExpr(G,A,bool), typeExpr(G,B,bool).
typeExpr(G,app(E,ES),T) :- typeExpr(G,E,types((X,T))),[A|B] = ES,

type(E,T) :- initCtx(G0), typeExpr(G0,E,T).

