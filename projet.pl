:- use_module(library(random)).
:- use_module(library(lists)).

%------------------------------PREDICATS DE BASE------------------------------
acteur(jmsalotti).
acteur(bpesquet).
acteur(eclermont).
acteur(vlespinet).
acteur(pafavier).
acteur(bleblanc).
acteur(saries).
acteur(cjauze).
acteur(jsaracco).
acteur(lgarcia).
acteur(bprebot).
acteur(bclaverie).
acteur(shourlier).
acteur(nrodriguez).
acteur(isese).
acteur(ceyraud).

policier(simsim).
policier(momo).
policier(ade).

personnage(X) :- policier(X); acteur(X).

tuile(X, Y, [], Sniper) :- X=<16, Y=<16, X>=1, Y>=1, (Sniper is 0;Sniper is 1).
tuile(X, Y, [T|Q], Sniper) :- personnage(T), tuile(X, Y, Q, Sniper).

getActeurs(Acteurs) :- findall(A, acteur(A), Acteurs).
getPoliciers(Policiers) :- findall(P, policier(P), Policiers).
getPersonnages(Personnages) :- getActeurs(Acteurs), getPoliciers(Policiers), append(Acteurs, Policiers, Personnages).

%------------------------------GENERATION ALEATOIRE DU PLATEAU------------------------------
comparerPosition(tuile(X, Y, _, _),tuile(X, Y, _, _)).

testerTuile(Tuile, [Tuile]).
testerTuile(Tuile, [T|Q]) :- comparerPosition(Tuile, T); testerTuile(Tuile, Q).

creerTuile(tuile(X, Y, _, 0)) :- random(1,16,X), random(1,16,Y).

tuileInvalide(tuile(X,Y,[], _), Liste) :- not(tuile(X,Y,[], _)) ; testerTuile(tuile(X,Y,[], _),Liste).

valideToutes([], _, []).
valideToutes([tuile(X, Y, [], S)|Q], Liste, [tuile(X, Y, [], S)|QT]):-
    tuile(X,Y,[], S),
    not(testerTuile(tuile(X, Y, [], _), Liste)),
    valideToutes(Q, Liste, QT).
valideToutes([tuile(X, Y, [], _)|Q], Liste, QT):-
    tuileInvalide(tuile(X, Y, [], _), Liste),
    valideToutes(Q, Liste, QT).

valideAdjacences(tuile(X, Y, _, S), Liste, ListeTuilesValides):-
    X1 is X + 1, Y1 is Y + 1, X2 is X - 1, Y2 is Y - 1,
    valideToutes([tuile(X1,Y,[], S), tuile(X,Y1,[], S), tuile(X2,Y,[], S), tuile(X,Y2,[], S)],Liste, ListeTuilesValides).

genererPlateau([],Plateau) :- !,creerTuile(Tuile), genererPlateau([Tuile], Plateau).
genererPlateau(L,L) :- length(L,16).
genererPlateau(L, Plateau) :-
    length(L, Size),
    Size < 16,
    repeat,
    random_member(tuile(X, Y, _, S), L),
    valideAdjacences(tuile(X, Y, _, S), L, ListeTuilesValides),
    random_member(NTuile, ListeTuilesValides),
    genererPlateau([NTuile|L], Plateau).

ajouterPersonnages([],[],[]).
ajouterPersonnages([TPlateau|QPlateau], [TPersonnages|QPersonnages], [T|QT]) :-
    ajouterPersonnage(TPlateau, TPersonnages, T),
    ajouterPersonnages(QPlateau, QPersonnages, QT).

ajouterPersonnage(tuile(X,Y,L, S), Acteur, tuile(X,Y,[Acteur|L], S)) :- tuile(X,Y,[Acteur|L], S).

setTuilesSniper(P, Arret, Arret, P).
setTuilesSniper([tuile(X,Y,Persos,_)|QPlateau], Compteur, Arret, [tuile(X,Y,Persos,1)|QTraitee]) :-
    NCompteur is Compteur+1,
    setTuilesSniper(QPlateau, NCompteur, Arret, QTraitee).

remplirPlateau(PlateauFinal) :-
    genererPlateau([], Plateau),
    getActeurs(Acteurs),
    ajouterPersonnages(Plateau, Acteurs, PlateauRempli),
    setTuilesSniper(PlateauRempli, 0, 8, PlateauFinal).

%------------------------------DEPLACER UN PERSONNAGE-----------------------------
majPlateau(tuile(X,Y,NL, S), [tuile(X1,Y1,_, _)|AQP], [tuile(X,Y,NL, S)|AQP]):- comparerPosition(tuile(X, Y, _, _),tuile(X1,Y1,_, _)).
majPlateau(tuile(X,Y,NL, S), [ATP|AQP], [ATP|NQPT]):- majPlateau(tuile(X,Y,NL, S), AQP, NQPT).

retirerPersonnage(tuile(X,Y,[], S), _, tuile(X,Y,[], S)).
retirerPersonnage(tuile(X,Y,[Acteur|Q], S), Acteur, tuile(X,Y,Q, S)).
retirerPersonnage(tuile(X,Y,[T|Q], S), Acteur, tuile(X,Y,[T|NQ], S)) :- retirerPersonnage(tuile(X,Y,Q,S), Acteur, tuile(X, Y, NQ, S)).

persoSurTuile(tuile(_,_,[Personnage|_], _), Personnage).
persoSurTuile(tuile(X,Y,[_|Q], S), Personnage) :- persoSurTuile(tuile(X,Y,Q, S), Personnage).

chercherTuile(T,[TPlateau|_], TPlateau) :- comparerPosition(T,TPlateau).
chercherTuile(T,[_|QPlateau], TuileResultat) :- chercherTuile(T,QPlateau,TuileResultat).

chercherTuileDePersonnage(Personnage, [TPlateau | _], TPlateau) :- persoSurTuile(TPlateau, Personnage).
chercherTuileDePersonnage(Personnage, [_ |QPlateau], TuileResultat) :- chercherTuileDePersonnage(Personnage, QPlateau, TuileResultat).

deplacerPersonnage(Personnage, T, Plateau, NPLateau) :- 
    chercherTuileDePersonnage(Personnage, Plateau, TuileInitiale),
    retirerPersonnage(TuileInitiale, Personnage, TuileSansPerso),
    majPlateau(TuileSansPerso, Plateau, PlateauInter),
    chercherTuile(T, Plateau, TR),
    ajouterPersonnage(TR, Personnage, TuileAvecAjout),
    majPlateau(TuileAvecAjout, PlateauInter, NPLateau).


%------------------------------TUER------------------------------
contientPolicier(Tuile,[Policier]):- persoSurTuile(Tuile,Policier).
contientPolicier(Tuile,[Policier|Q]):- persoSurTuile(Tuile,Policier);
                                            contientPolicier(Tuile,Q).
verifierAlentours([],_).
verifierAlentours([T|Q],Policiers):- not(contientPolicier(T,Policiers)),
                                     verifierAlentours(Q,Policiers).

trouverTuilesDsPlateau(Positions,Plateau,X,Y,Personnages) :- 
    member((X,Y),Positions),
    member(tuile(X,Y,Personnages, _),Plateau).

getTuilesDsPlateau(Positions,Plateau,R) :- 
    findall(tuile(X,Y,Personnages, _),
    trouverTuilesDsPlateau(Positions,Plateau,X,Y,Personnages),R).

trouverAdjacencesValides(X,Y,Plateau,R) :- 
    X1 is X+1,X2 is X-1,Y1 is Y+1,Y2 is Y-1,
    getTuilesDsPlateau([(X,Y),(X1,Y),(X2,Y),(X,Y1), (X,Y2)],Plateau,R).

champsLibreMeurtre(tuile(X, Y, _, _),Plateau) :- 
    trouverAdjacencesValides(X,Y,Plateau,Valides),                                            
    getPoliciers(Policiers),
    verifierAlentours(Valides,Policiers).

tuerCouteau(Tueur,Cible, Plateau) :- 
    chercherTuileDePersonnage(Tueur, Plateau, Tuile),
    chercherTuileDePersonnage(Cible, Plateau, Tuile).

tuerFusil(Tueur, Cible, Plateau) :- 
    chercherTuileDePersonnage(Tueur, Plateau, tuile(X,Y, Personnages, _)),
    trouverAdjacencesValides(X,Y,Plateau,Voisins),
    chercherTuileDePersonnage(Cible, Plateau, TuileCible),
    testerTuile(TuileCible, Voisins),
    length(Personnages, L),
    L<2.

tuerSniper(Tueur, Cible, Plateau) :- 
    chercherTuileDePersonnage(Tueur, Plateau, tuile(X, _, Personnages, S)),
    chercherTuileDePersonnage(Cible, Plateau, tuile(X, _,  _, _)),
    length(Personnages, L),
    L<2,
    S>0.
tuerSniper(Tueur, Cible, Plateau) :- 
    chercherTuileDePersonnage(Tueur, Plateau, tuile(_, Y, Personnages, S)),
    chercherTuileDePersonnage(Cible, Plateau, tuile(_, Y,  _, _)),
    length(Personnages, L),
    L<2,
    S>0.

peutTuer(Tueur, Cible, Plateau) :- 
    chercherTuileDePersonnage(Tueur, Plateau, TuileTueur),
    champsLibreMeurtre(TuileTueur, Plateau),
    (tuerCouteau(Tueur, Cible, Plateau); tuerFusil(Tueur, Cible, Plateau); tuerSniper(Tueur, Cible, Plateau)).

eliminerDuPlateau(Cible,Plateau,PlateauMaj):- 
    chercherTuileDePersonnage(Cible, Plateau, TuileInitiale),
    retirerPersonnage(TuileInitiale, Cible, TuileSansPerso),
    majPlateau(TuileSansPerso, Plateau, PlateauMaj).



%------------------------------JOUEURS------------------------------
creerJoueur(joueur(_, [], 0, Nom)) :-
    write('Entrer un pseudo'),
    read(Nom).

creerJoueurs([], NbJoueurs, ListeFinale) :- !, creerJoueur(J), creerJoueurs([J], NbJoueurs, ListeFinale).
creerJoueurs(L, NbJoueurs, L) :- length(L, NbJoueurs).
creerJoueurs(L, NbJoueurs, ListeFinale) :-
    length(L, Size),
    Size < NbJoueurs,
    creerJoueur(NJoueur),
    creerJoueurs([NJoueur|L], NbJoueurs, ListeFinale).

attribuerCibles(ListeJoueursInitialisee, ListeJoueurs) :-
    getActeurs(Acteurs),
    random_permutation(Acteurs, ActeursMelanges),
    choisirCiblesJoueurs(ActeursMelanges, ListeJoueursInitialisee, ListeJoueurs).

choisirCiblesJoueurs(_,[],[]).
choisirCiblesJoueurs([A1, A2, A3, A4 | Q], [joueur(_, [] , NbCiblesTuees , Nom)|QJI], [joueur(A1, [A2, A3, A4], NbCiblesTuees , Nom)|QJ]) :-
    choisirCiblesJoueurs(Q, QJI, QJ).

genererJoueurs(ListeResultat) :-
    repeat,
    write('Entrer le nombre de joueurs : '),
    read(NbJoueurs),
    NbJoueurs < 5,
    NbJoueurs > 0,
    creerJoueurs([], NbJoueurs, ListeJoueursCrees),
    attribuerCibles(ListeJoueursCrees, ListeResultat).


%------------------------------TOURS DE JEU------------------------------
demarrer() :- 
    write('\n\nIl regne un climat de tension a CognitoTown, la ville au taux de criminalite le plus eleve au monde ! \n'),
    write('Parviendrez vous a tuer vos cibles en toute discretion ? C est ce que nous allons voir... \n'),
    write('Mais prenez garde, les policiers ade, simsim et momo veillent au grain. \n\n'),
    genererJoueurs(Joueurs), 
    write('\n-------------------------------------------JOUEURS-------------------------------------------\n\n'),
    afficherJoueurs(Joueurs),
    write('---------------------------------------------------------------------------------------------\n\n'),
    remplirPlateau(Plateau), 
    jouer(Joueurs, Plateau).

jouer([joueur(Tueur, [Cible1,Cible2,Cible3], NbCiblesTuees, Nom)|Qj], Plateau) :-
  tour(joueur(Tueur, [Cible1,Cible2,Cible3], NbCiblesTuees, Nom), Qj, Plateau, 0, 0, PlateauFinal, JoueurActualise),
  append(Qj, [JoueurActualise], ListeConcatenee),
  jouer(ListeConcatenee, PlateauFinal).

tour(JoueurFinal, _, PlateauFinal, 2, _, PlateauFinal, JoueurFinal) :- estGagnant(JoueurFinal).
tour(joueur(Tueur,[Cible1,Cible2,Cible3], NbCiblesTuees, Nom), Adv, Plateau, Rang, ATue, PlateauFinal, JoueurFinal):-
  write('\n-------------------------------------------PLATEAU-------------------------------------------\n\n'),
  afficherPlateau(Plateau),
  write('\n---------------------------------------------------------------------------------------------\n\n'),
  write('Action numero '),
  write(Rang),
  write(' de '),
  write(Nom),
  write(' :\n'),
  write(Nom),
  gererAction(joueur(Tueur,[Cible1,Cible2,Cible3], NbCiblesTuees, Nom), Plateau, Adv, ATue, NouveauPlateau, JoueurActualise, ATueMaj),
  NRang is Rang+1,
  estGagnant(joueur(Tueur, [Cible1,Cible2,Cible3], NbCiblesTuees, Nom)), 
  tour(JoueurActualise, Adv, NouveauPlateau, NRang, ATueMaj, PlateauFinal, JoueurFinal).


gererAction(Joueur,Plateau, [joueur(TueurAdv, CiblesAdv, NbCiblesAdv, NomAdv)|Q], 1, NouveauPlateau, JoueurActualise, 1) :- 
  repeat,
  write(', que voulez vous faire ?\n'),
  write('d pour deplacer\n'),
  write('i pour controler une identite\n'),
  read(Action),
  (Action == d; Action == i),
  (Action == d -> deplacer(Joueur, Plateau, NouveauPlateau, JoueurActualise);
    gererInformations(Joueur, [joueur(TueurAdv, CiblesAdv, NbCiblesAdv, NomAdv)|Q], Plateau, NouveauPlateau, JoueurActualise)
    ).

gererAction(Joueur,Plateau, [joueur(TueurAdv, CiblesAdv, NbCiblesAdv, NomAdv)|Q], 0, NouveauPlateau, JoueurActualise, ATueMaj) :- 
  repeat,
  write(', que voulez vous faire ?\n'),
  write('t pour tuer\n'),
  write('d pour deplacer\n'),
  write('i pour controler une identite\n'),
  read(Action),
  (Action == t; Action == d; Action == i),
  (Action == t -> (tuer(Joueur,Plateau, NomAdv, NouveauPlateau, JoueurActualise), ATueMaj is 1);
    (Action == d -> (deplacer(Joueur, Plateau,NouveauPlateau, JoueurActualise), ATueMaj is 0);
      (gererInformations(Joueur, [joueur(TueurAdv, CiblesAdv, NbCiblesAdv, NomAdv)|Q], Plateau, NouveauPlateau, JoueurActualise),
      ATueMaj is 0)
    )
  ).

estGagnant(joueur(_, _, NbCiblesTuees, _)) :- NbCiblesTuees == 3 -> abort; true. 


%------------------------------GESTION DES ACTIONS------------------------------

%------Demande d informations------
demanderInformations(Adv,Plateau,PlateauMaj) :-
    repeat,
    write('Quel perso pensez vous etre un tueur : '),
    read(Perso),
    getActeurs(Acteurs),
    member(Perso,Acteurs),
    chercherTuileDePersonnage(Perso,Plateau,TuileResultat),
    getPoliciers(Policiers),
    contientPolicier(TuileResultat,Policiers),
    !,

    repeat,
    write('A quel joueur pensez vous qu il appartient : '),
    read(NomJoueur),
    trouverJoueurAvecNom(NomJoueur, Adv, joueur(Tueur, _ , _, _)), 
    !,
    Tueur == Perso,
    write(Perso),
    write(" est demasque.e et ne peut donc plus tuer !\n"),
    eliminerDuPlateau(Perso,Plateau,PlateauMaj).

gererInformations(Joueur, Adv, Plateau, PlateauMaj, Joueur):-
    demanderInformations(Adv, Plateau, PlateauMaj),
    write("Felicitations !\n").
gererInformations(Joueur, _, Plateau, Plateau, Joueur):- 
    write("Pas de chance \n").

trouverJoueurAvecNom(Nom, [joueur(Tueur,Cibles, NbCiblesTuees, Nom)], joueur(Tueur,Cibles, NbCiblesTuees, Nom)).
trouverJoueurAvecNom(Nom, [joueur(Tueur,Cibles, NbCiblesTuees, Nom)|_], joueur(Tueur,Cibles, NbCiblesTuees, Nom)).
trouverJoueurAvecNom(Nom, [_|Qjoueurs], JoueurRes):- trouverJoueurAvecNom(Nom, Qjoueurs, JoueurRes).


%------Déplacer un personnage------
deplacerActeur(Perso,Plateau, X, Y, NouveauPlateau) :-
  chercherTuileDePersonnage(Perso,Plateau,_),
  member(tuile(X,Y,_,_),Plateau),
  deplacerPersonnage(Perso,tuile(X,Y,_,_),Plateau,NouveauPlateau).

deplacerPolicier(Perso, Plateau, X, Y, NouveauPlateau):-
  deplacerActeur(Perso, Plateau, X, Y, NouveauPlateau);
  member(tuile(X,Y,Persos,S),Plateau),
  ajouterPersonnage(tuile(X,Y,Persos,S), Perso, TuileResultat),
  majPlateau(TuileResultat,Plateau,NouveauPlateau).

deplacer(Joueur, Plateau,NouveauPlateau, Joueur):-
  repeat,
  write('\nQui deplacer ?'),
  read(Perso),
  getActeurs(Acteurs),
  getPoliciers(Policiers),
  !,
  repeat,
  write('Ou voulez-vous deplacer ce perso ? \n'),
  write(' Coordonnee X de la case (ex : "14.") : '),
  read(X),
  write(' Coordonnee Y de la case (ex : "14.") : '),
  read(Y),
  ((member(Perso,Acteurs) -> deplacerActeur(Perso, Plateau, X, Y, NouveauPlateau));
  (member(Perso,Policiers) -> deplacerPolicier(Perso, Plateau, X, Y, NouveauPlateau))).


%------Tuer un personnage------
tuer(joueur(Tueur, [Cible1, Cible2, Cible3], NbCiblesTuees, Nom), Plateau, NomAdv, PlateauMaj, JoueurActualise) :-
  choisirCible(Plateau,Cible),!,
  peutTuer(Tueur,Cible,Plateau),
  chercherTuileDePersonnage(Cible, Plateau, tuile(X, Y, Persos, Sniper)), 
  eliminerDuPlateau(Cible, Plateau, PlateauIntermediaire),
  retirerPersonnage(tuile(X, Y, Persos, Sniper), Cible, TuileResultat),
  write(Cible), write( ' est mort.e !!! \n\n' ),
  deplacerPopulation(TuileResultat, PlateauIntermediaire, NomAdv, PlateauMaj),
  incrementerCiblesTuees(Cible, joueur(Tueur, [Cible1, Cible2, Cible3], NbCiblesTuees, Nom), JoueurActualise).

choisirCible(Plateau,Cible) :-
  repeat,
  write('\nEntrez le nom de votre cible: '),
  read(Cible),
  getActeurs(Acteurs),
  member(Cible,Acteurs),
  chercherTuileDePersonnage(Cible,Plateau,_).

deplacerPopulation(tuile(X, Y, Persos, _), Plateau, NomAdv, PlateauMaj) :-
  repeat, 
  write(NomAdv),
  write(', choisissez un policier (simsim, ade ou momo): '),
  read(Policier),
  getPoliciers(Policiers), 
  member(Policier, Policiers), 
  !,
  deplacerTemoins(Persos, Plateau, PlateauInter),
  deplacerPolicier(Policier, PlateauInter, X, Y, PlateauMaj).

deplacerTemoins([], PlateauFinal, PlateauFinal).
deplacerTemoins([Temoin|Q], Plateau, PlateauFinal) :-
  write('Veuillez deplacer '), 
  write(Temoin),
  write('\n'),
  repeat, 
  write(' Ou voulez-vous deplacer ce perso ? \n'),
  write(' Coordonnee X de la case (ex : "14.") : '),
  read(X),
  write(' Coordonnee Y de la case (ex : "14.") : '),
  read(Y),
  deplacerActeur(Temoin, Plateau, X, Y, PlateauMaj),
  deplacerTemoins(Q, PlateauMaj, PlateauFinal).

incrementerCiblesTuees(_, joueur(Tueur, Cibles, NbCiblesTuees, Nom), joueur(Tueur, Cibles, NbCiblesTuees, Nom)).
incrementerCiblesTuees(Cible, joueur(Tueur, Cibles, NbCiblesTuees, Nom), joueur(Tueur, Cibles, NNbCiblesTuees, Nom)) :-
  member(Cible, Cibles),
  NNbCiblesTuees is NbCiblesTuees + 1.

%------------------------------AFFICHAGE------------------------------
afficherPlateau([]).
afficherPlateau([tuile(X,Y,Persos,S)|Q]) :-
    display(tuile(X,Y,Persos,S)), 
    write('\n'),
    afficherPlateau(Q).

afficherJoueurs([]).
afficherJoueurs([joueur(Tueur, [Cible1, Cible2, Cible3], _, Nom)|Q]):-
    write(Nom),
    write(', vous avez comme Tueur : '),
    write(Tueur),
    write(' et vous devrez abattre : '),
    write(Cible1), 
    write(', '),
    write(Cible2), 
    write(' et '),
    write(Cible3),
    write('\n\n'),
    afficherJoueurs(Q).




%============================PREDICATS DE TEST================================
plateauTest([
    tuile(1,2,[bpesquet], 0),
    tuile(2,2,[jmsalotti,bprebot,momo], 0),
    tuile(1,3,[vlespinet], 1),
    tuile(3,2,[bclaverie], 0),
    tuile(3,1,[shourlier], 1),
    tuile(3,3,[isese, nrodriguez, lgarcia, cjauze], 0),
    tuile(4,2,[saries], 1),
    tuile(4,3,[pafavier, ceyraud], 1),
    tuile(5,2,[bleblanc], 0),
    tuile(6,2,[ade],0)
    ]).

demarrerTest():- 
    genererJoueurs(Joueurs), 
    display(Joueurs), 
    plateauTest(P), 
    jouer(Joueurs, P).
