:- use_module(library(pce)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- [projet].

ask_employee :-
    new(Dialog, dialog('Define employee')),
    send_list(Dialog, append,
    [ new(N1, text_item(first_name)),
    new(N2, text_item(family_name)),
    new(S, new(S, menu(sex))),
    new(A, int_item(age, low := 18, high := 65)),
    new(D, menu(department, cycle)),
    button(cancel, message(Dialog, destroy)),
    button(enter, and(message(@prolog,
    assert_employee,
    N1?selection,
    N2?selection,
    S?selection,
    A?selection,
    D?selection),
    message(Dialog, destroy)))
    ]),
    send_list(S, append, [male, female]),
    send_list(D, append, [research, development, marketing]),
    send(Dialog, default_button, enter),
    send(Dialog, open).

assert_employee(FirstName, FamilyName, Sex, Age, Depth) :-
    format('Adding ˜w ˜w ˜w, age ˜w, working at ˜w˜n',
    [ Sex, FirstName, FamilyName, Age, Depth]).

test :-  new(D, dialog('Layout Demo 1')),
   send(D, append, new(BTS, dialog_group(buttons, group))),
   send(BTS, gap, size(0, 30)),
   send(BTS, append, button(add)),
   send(BTS, append, button(rename), below),
   send(BTS, append, button(delete), below),
   send(BTS, layout_dialog),
   send(D, append, new(LB, list_browser), right),
   send(D, append, new(TI, text_item(name, ''))),
   send(LB, alignment, left),
   send(D, layout),
   send(LB, bottom_side, BTS?bottom_side),
   send(LB, right_side, TI?right_side),
   send(D, open).

end(D) :- send(D, destroy).
test(P) :- display(P).

getX([],[]).
getX([tuile(X,_,_)|Q],[X|QT]) :- getX(Q,QT).

getY([],[]).
getY([tuile(_,Y,_)|Q],[Y|QT]) :- getY(Q,QT).

findMinX(P,Min) :- getX(P,Xs), min_list(Xs,Min).
findMinY(P,Min) :- getY(P,Ys), min_list(Ys,Min).

afficherPlateau(W) :- send(W,clear),
                      genererPlateau([], P),
                      findMinX(P, MinX), findMinY(P,MinY),
                      writeln(MinX), writeln(MinY),
                      drawBoard(P, W, MinX, MinY),
                      writeln(P).

drawBoard([], _, _, _).
drawBoard([T|Q], Window, MinX, MinY) :- drawTile(T,Window,MinX,MinY), drawBoard(Q,Window,MinX, MinY).

drawTile(tuile(X,Y,_),W,MinX, MinY) :- send(W, display, new(_, box(50, 50)), point((X-MinX)*51, (Y-MinY)*51)).

start :- new(@MainMenu, frame('Menu principal')),
      % Setting up the window placements and display
      send(@MainMenu, append, new(@BoardDisplay, window('',size(600,600)))),
      send(@MainMenu, append, new(@ControlPanel, dialog('', size(600,100)))),
      send(@MainMenu, append, new(@DataPanel, dialog('', size(200,600)))),
      send(@ControlPanel, below, @BoardDisplay),
      send(@DataPanel, right, @BoardDisplay),
      %send(@MainMenu, display, new(@Canvas, box(100,100))),

      send(@ControlPanel, append, new(CZone, dialog_group(buttons, group))),
      send(CZone, gap, size(30, 0)),
      send(CZone, append, button(quit, message(@prolog, end, @MainMenu))),
      send(CZone, append, button(create, message(@prolog, afficherPlateau, @BoardDisplay)), right),

      %Start test zone
      %send(@BoardDisplay, display, new(_, box(100,100)), point(0,0)),

      send(@MainMenu, open).

role :- new(D, dialog('fenetre test')),
    new(T, text_item(entrez)),
    send(D, append, T),
    send(D, append, button(ok,
    message(@prolog, print, T))),
    send(D, append, button(close,
    message(@prolog, end, D))),
    send(D, open).
