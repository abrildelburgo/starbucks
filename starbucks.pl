%cafe(PorcentajeDeCafe, PorcentajeDeLeche).
%te(GustoDeTe).
%smoothie(GustoDeSmoothie).
%frapuccino(Composicion).

%bebida(Bebida, Temperatura, tipoDeBebida).
bebida(teManzanilla, caliente, te(manzanilla)).
bebida(frapuccinoOreo, frio, frapuccino([leche, cafe, chocolate, oreo])).
bebida(lagrima, caliente, cafe(2, 98)).
bebida(cortado, caliente, cafe(50, 50)).
bebida(smoothieFrutilla, frio, smoothie(frutilla))

%precio(Bebida, Precio).
precio(teManzanilla, 70).
precio(frapuccinoOreo, 90).
precio(lagrima, 50).
precio(cortado, 35).
precio(smoothieFrutilla, 80).

%pedido(Cliente, Bebida).
pedido(isaacAsimov, cortado).
pedido(stephenKing, teManzanilla).
pedido(stephenKing, cortado).
pedido(neilGaiman, frapuccinoOreo).
pedido(neilGaiman, lagrima).
pedido(neilGaiman, teManzanilla).


bebida(frapuccinoCocoYDDL,frio,frapuccino([leche,cafe,coco,dulceDeLeche])).   %PUNTO1
bebida(teFrutosRojos,caliente,te(frutosRojos)).

precio(frapuccinoCocoYDDL,75).
precio(teFrutosRojos,40).

esCopado(Cliente):-                      %PUNTO2
 pedido(Cliente,_),
 compraSiempreBebidasCopadas(Cliente),
 not(pidioTe(Cliente)).

compraSiempreBebidasCopadas(Cliente):-
 forall(pedido(Cliente,Bebida),esBebidaCopada(Bebida)).

esBebidaCopada(Bebida):-                    %DELEGAR MAAAAAAAS
 bebida(Bebida,_,cafe(_,_)).
esBebidaCopada(Bebida):-
 bebida(Bebida,_,frapuccino(Ingredientes)),
 member(chocolate,Ingredientes).
esBebidaCopada(Bebida):-
 bebida(Bebida,_,smoothie(_)).

pidioTe(Cliente):-
 pedido(Cliente,Te),
 bebida(Te,_,te(_)).

esMami(Cliente):-                     %PUNTO3
 pedido(Cliente,_),
 forall(pedido(Cliente,Bebida),esBebidaMami(Bebida)).   %hace falta delegar esBebidaFria?

esBebidaMami(Bebida):-
 bebida(Bebida,frio,_).
esBebidaMami(Bebida):-
 bebida(Bebida,_,cafe(Cafe,Leche)),
 Leche>Cafe.

clienteRegular(Cliente):-           %PUNTO4
 pedido(Cliente,_),
 gastoMasDe600(Cliente).

gastoMasDe600(Cliente):-
 findall(Precio,gasto(Cliente,Precio),ListaDineroGastado),
 sumlist(ListaDineroGastado,TotalGastado),
 TotalGastado>600.

gasto(Cliente,Precio):-
 pedido(Cliente,Bebida),
 precio(Bebida,Precio)).

precio2(Bebida,Tamanio,Precio):-          %PUNTO5
 precio(Bebida,ValorAlto),
 recargo(Tamanio,Recargo),
 Precio is ValorAlto+ValorAlto*Recargo.

recargo(alto,0).
recargo(grande,0.1).
recargo(venti,0.25).

publicoDeseable(ListaClientes):-    %PUNTO6
 member(_,ListaClientes),
 forall(consumieronEnLocal(ListaClientes),esClienteDeseable(Cliente)).

consumieronEnLocal(ListaClientes):-
 member(Cliente,ListaClientes).

esClienteDeseable(Cliente):-
 esCopadoORegular(Cliente),
 not(esMami(Cliente)).

esCopadoORegular(Cliente):-
 esCopado(Cliente).
esCopadoORegular(Cliente):-
 clienteRegular(Cliente)
 

precio2(Bebida,Tamanio,Precio):-          %PUNTO7
 precio(Bebida,ValorAlto),
 recargo(Tamanio,Recargo),
 Precio is ValorAlto+ValorAlto*Recargo.
precio2(Bebida,venti,Precio):-
 precio2(Bebida,grande,ValorGrande),
 Precio is ValorGrande*1.25.

recargo(alto,0).
recargo(grande,0.1).
