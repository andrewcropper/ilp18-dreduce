:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(pairs)).
:- use_module(library(timeout)).
:- use_module(library(random)).
:- use_module(library(system)).

set_rand:-
  datime(datime(_,_Month,_Day,_H,Minute,Second)),
  X is Minute * Second,Y=X,Z=X,
  setrand(rand(X,Y,Z)).

main:-
    set_rand,
    %% between(1,500,TrainPos),
    between(1,1000,TrainPos),
    gen_train(Train),
    pprint_train(Train,TrainPos),
    false.
main.

gen_train(train(NCarriages,Carriages)):-
    random_member(NCarriages,[2,3,4]),
    length(Carriages,NCarriages),
    maplist(gen_carriage,Carriages).

gen_carriage(c(CarSize,NumWheels,RoofClosed,RoofShape,NLoads,Loads)) :-
    car_size(CarSize),
    wheels(CarSize,NumWheels),
    car_shape(CarSize,CarShape),
    roof(CarShape,RoofClosed),
    roof_shape(CarSize,CarShape,RoofClosed,RoofShape),
    load_size(CarSize,NLoads),
    length(Loads,NLoads),
    gen_loads(Loads,CarSize).

gen_loads([],_).
gen_loads([load(LoadSize,LoadShape)|T],CarSize):-
    load_size(CarSize,LoadSize),
    load_shape(CarSize,LoadShape),
    gen_loads(T,CarSize).

car_size(CarSize):-
    random_member(CarSize,[short,long]).

wheels(short,2).
wheels(long,NumWheels) :-
    random_member(NumWheels,[2,3]).

car_shape(short,Shape):-
    random_member(Shape,[rectangle,u_shaped,bucket_shaped,hexagon,elipse]). % triangle,circle?
car_shape(long,rectangle).

roof(hexagon,closed):-!.
roof(elipse,closed):-!.
roof(_,Roof):-random_member(Roof,[open,closed]).

roof_shape(long,_CarShape,closed,RoofShape):-!,random_member(RoofShape,[flat,jagged]).
roof_shape(_,hexagon,closed,flat) :-!.
roof_shape(_,elipse,closed,arc):-!.
roof_shape(short,_,closed,RoofShape):-!,random_member(RoofShape,[flat,peaked]).

roof_shape(_,_,open,open).

load_size(short,LoadSize):-!, random_member(LoadSize,[1,2]).
load_size(long,LoadSize):-!, random_member(LoadSize,[0,1,2,3]).

load_shape(short,LoadShape):-!,
    random_member(LoadShape,[circle, triangle, rectangle, diamond]).
load_shape(long,LoadShape):-!,
    random_member(LoadShape,[circle, inverted_triangle, hexagon, rectangle]).

pprint_train(train(NCarriages,Carriages),TrainPos):-
    atomic_list_concat([t,TrainPos],TrainGuid),
    pprint_literal(train(TrainGuid)),
    %% pprint_literal(num_carriages(TrainGuid,NCarriages)),
    pprint_cars(Carriages,TrainGuid,1).

pprint_cars([],_GUID,_CarPos).
pprint_cars([c(CarSize,NumWheels,RoofClosed,RoofShape,_NLoads,Loads)|T],TrainGuid,CarPos):-
    atomic_list_concat([TrainGuid,'_',c,CarPos],CarGuid),
    pprint_literal(has_car(TrainGuid,CarGuid)),
    pprint_car_size(CarSize,CarGuid),
    pprint_car_wheels(NumWheels,CarGuid),
    pprint_car_roof(RoofClosed,CarGuid),
    pprint_car_roof_shape(RoofShape,CarGuid),
    pprint_car_loads(Loads,CarGuid,1),
    NextCarPos is CarPos+1,
    pprint_cars(T,TrainGuid,NextCarPos).

pprint_car_loads([],_,_).
pprint_car_loads([load(LoadSize,LoadShape)|T],CarGuid,LoadPos):-
    atomic_list_concat([CarGuid,'_',l,LoadPos],LoadGuid),
    pprint_literal(has_load(CarGuid,LoadGuid)),
    pprint_load_size(LoadSize,LoadGuid),
    pprint_load_shape(LoadShape,LoadGuid),
    NextLoadPos is LoadPos+1,
    pprint_car_loads(T,CarGuid,NextLoadPos).

pprint_load_size(0,LoadGuid):-
    pprint_literal(zero_load(LoadGuid)).
pprint_load_size(1,LoadGuid):-
    pprint_literal(one_load(LoadGuid)).
pprint_load_size(2,LoadGuid):-
    pprint_literal(two_load(LoadGuid)).
pprint_load_size(3,LoadGuid):-
    pprint_literal(three_load(LoadGuid)).

pprint_load_shape(circle,LoadGuid):-
    pprint_literal(circle(LoadGuid)).
 pprint_load_shape(triangle,LoadGuid):-
    pprint_literal(triangle(LoadGuid)).
pprint_load_shape(diamond,LoadGuid):-
    pprint_literal(diamond(LoadGuid)).
pprint_load_shape(inverted_triangle,LoadGuid):-
    pprint_literal(inverted_triangle(LoadGuid)).
pprint_load_shape(hexagon,LoadGuid):-
    pprint_literal(hexagon(LoadGuid)).
pprint_load_shape(rectangle,LoadGuid):-
    pprint_literal(rectangle(LoadGuid)).

pprint_car_size(short,CarGuid):-
    pprint_literal(short(CarGuid)).
pprint_car_size(long,CarGuid):-
    pprint_literal(long(CarGuid)).

pprint_car_wheels(1,CarGuid):-
    pprint_literal(one_wheel(CarGuid)).
pprint_car_wheels(2,CarGuid):-
    pprint_literal(two_wheels(CarGuid)).
pprint_car_wheels(3,CarGuid):-
    pprint_literal(three_wheels(CarGuid)).

pprint_car_roof(open,CarGuid):-
    pprint_literal(roof_open(CarGuid)).
pprint_car_roof(closed,CarGuid):-
    pprint_literal(roof_closed(CarGuid)).

pprint_car_roof_shape(open,_):-!.
pprint_car_roof_shape(flat,CarGuid):-
    pprint_literal(roof_flat(CarGuid)).
pprint_car_roof_shape(jagged,CarGuid):-
    pprint_literal(roof_jagged(CarGuid)).
pprint_car_roof_shape(arc,CarGuid):-
    pprint_literal(roof_arc(CarGuid)).
pprint_car_roof_shape(peaked,CarGuid):-
    pprint_literal(roof_peaked(CarGuid)).

pprint_literal(L):-
    format('~w.\n',L).


%% 1. A train has two, three or four cars, each of which can either be long or short.
%% 2. A long car can have either two or three axles.
%% 3. A short car can be rectangular, u-shaped, bucket-shaped, hexagonal, or elliptical, while a long car must be rectangular.
%% 4. A hexagonal or elliptical car is necessarily closed, while any other car can be either open or closed.
%% 5. The roof of a long closed car can be either flat or jagged.
%% 6. The roof of a hexagonal car is necessarily flat, while the roof of an elliptical car is necessarily an arc. Any other short closed car can have either a flat of a peaked roof.
%% 9. A short car contains either one or two replicas of the following kinds of load: circle, triangle, rectangle, diamond.
%% 8. A long car can be empty if it can contain one, two or three replicas of one of the following kinds of load: circle, inverted-triangle, hexagon, rectangle.
%% 7. If a short car is rectangular then it can also be double-sided. ??
%% 10. No sub-distinctions are drawn among rectangular loads, even though some are drawn square and others more or less oblong. The presumption is that they are drawn just as oblong as they need to be in each case to fill the available container space.

random_member(E,L):-
    length(L,N1),
    N2 is N1+1,
    random(1,N2,K),
    nth1(K,L,E).