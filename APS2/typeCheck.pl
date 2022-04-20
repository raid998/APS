assocArg([],List,List).
assocArg([arg(id(X),Y)|Tail],List,[(X,Y)|Rest]) :- assocArg(Tail,List,Rest).

assocArgp([],List,List).
assocArgp([argp(id(X),Y)|Tail],List,[(X,Y)|Rest]) :- assocArgp(Tail,List,Rest).

assoc(X, [(X,V)|_], V).
assoc(X, [_|XS], V) :- assoc(X, XS, V).

append([],Ys,Ys).
appen([X|Xs],Ys,[X|Zs]) :- append(Xs,Ys,Zs).

initCtx([(true,bool),(false,bool),(not,types([bool],bool)),(eq,types([int,int],bool)),(lt,types([int,int],bool)),(add,types([int,int],int)),(sub,types([int,int],int)),(mul,types([int,int],int)),(div,types([int,int],int))]).

typeCheck(G,[E],[T]) :- typeExpr(G,E,T).
typeCheck(G,[E|ES],[T|TS]) :- typeExpr(G,E,T),typeCheck(G,ES,TS).

argCheck(G,[arg(X,Y)|_]) :- typeExpr(G,X,Y).
argCheck(G,[_|T]) :- argCheck(G,T).

genTypes([],TS,TS).
genTypes([arg(_,T)|ARGS],TS,[T|REST]) :- genTypes(ARGS,TS,REST).

genTypesp([],TS,TS).
genTypesp([argp(_,T)|ARGS],TS,[T|REST]) :- genTypesp(ARGS,TS,REST).

/* SUITE DE COMMANDES */
typeSeq(G,[X],void) :- typeStat(G,X,void).
typeSeq(G,[C|CS],void) :- (typeDef(G,C,G1),typeSeq(G1,CS,void));(typeStat(G,C,void),typeSeq(G,CS,void)).


/* PROG*/

typeProg(prog(P),void) :- initCtx(G0),typeBloc(G0,P,void).

/* DEFS */

typeDef(G,const(id(X),T,E),[(X,T)|G]) :- typeExpr(G,E,T).

typeDef(G,funDef(id(X),T,args(A),E),[(X,types(TS,T))|G]) :- assocArg(A,G,G1), typeExpr(G1,E,T),genTypes(A,[],TS).

typeDef(G,funRecDef(id(X),T,args(A),E),[(X,types(TS,T))|G]) :- genTypes(A,[],TS), assocArg(A,G,G1), typeExpr([(X,types(TS,T))|G1],E,T).

typeDef(G,var(id(X),int),[(X,ref(int))|G]).

typeDef(G,var(id(X),bool),[(X,ref(bool))|G]).

typeDef(G,proc(id(X),argsp(A),Bk),[(X,types(TS,void))|G])      :- assocArgp(A,G,G1),genTypesp(A,[],TS),typeBloc(G1,Bk,void).

typeDef(G,procRec(id(X),argsp(A),Bk),[(X,types(TS,void))|G]) :- assocArgp(A,G,G1),genTypesp(A,[],TS),typeBloc([(X,types(TS,void))|G1],Bk,void).

/* BLOC */

typeBloc(G,C,void) :- typeSeq(G,C,void).


/* INSTRUCTION  */

typeStat(G,echo(E),void) :- typeExpr(G,E,int).

typeStat(G,set(id(X),E),void) :- assoc(X,G,ref(T)),typeExpr(G,E,T).

typeStat(G,iff(E,Bk1,Bk2), void) :- typeExpr(G,E,bool),typeBloc(G,Bk1,void), typeBloc(G,Bk2,void).

typeStat(G,while(E,Bk),void) :- typeExpr(G,E,bool),typeBloc(G,Bk,void).

typeStat(G,call(id(X),ES),void) :- assoc(X,G,types(TS,void)), typeCheck(G,ES,TS).


/* ENTIER */

typeExpr(_,num(_),int).

typeExpr(G,id(X),T):- assoc(X,G,ref(T)).

typeExpr(G,id(X),T) :- assoc(X,G,T).

typeExpr(G,if(C,B,A),T) :- typeExpr(G,C,bool),typeExpr(G,B,T),typeExpr(G,A,T).

typeExpr(G,and(A,B),bool) :- typeExpr(G,A,bool), typeExpr(G,B,bool).

typeExpr(G,or(A,B),bool) :- typeExpr(G,A,bool), typeExpr(G,B,bool).

typeExpr(G,app(E,ES),T) :- typeExpr(G,E,types(TA,T)),typeCheck(G,ES,TA).

typeExpr(G,fun(args(A),E),types(TS,T)) :- assocArg(A,G,NewG),typeExpr(NewG,E,T),genTypes(A,[],TS).

typeExpr(G,adr(X),ref(T)) :- assoc(X,G,ref(T)).

type(E,T) :- initCtx(G0), typeExpr(G0,E,T).

exitCode(ok) :- halt(0).
exitCode(_) :- halt(1).

main_stdin :-
	read(user_input,T),
	typeProg(T,R),
	print(R),
	nl,
	exitCode(R).