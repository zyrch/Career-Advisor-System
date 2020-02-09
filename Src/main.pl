%
% Zyrch - Rupanshu Yadav
%

%=================TOOLS==================%
% display text with delay between each character
delayText([H|T]) :- put_char(H), flush_output, sleep(0.1), delayText(T).
delayText([]).

displayText(K):-
  atom_chars(K, A),
  delayText(A).

% Clears the screen
clear:-
  format('~c~s', [0x1b, "[2J"]). % issue CSI 2 J.

loading:-
  write('Loading  '),
  clear,
  write('Loading  | '),
  clear,
  write('Loading  / ').

%============CODE STARTS HERE============%

main :-
  start,
  initialize,
  remove_prev,
  askall,
  initialize,
  compute,
  displayans.

displayans:-
  create_list(5, X),
  max_list(X, Max, Index),
  ( Max < 50 -> write('Sorry our Data Base does not have a career option for you.'), nl
   ;
  (mydata(List), NewIndex is 5 - Index,
  parse(NewIndex, List, Response),
  ( Response == software_developing -> write('According to our database you are best suited to be a Software Developer');
    Response == data_scientist -> write('According to our database you are best suited to be a data scientist');
    Response == it_consult -> write('According to our database you are best suited to be a It consultant');
    Response == machine_learning -> write('According to our database you are best suited to have a career in Machine Learning and Aritifical Intelligence');
    Response == research -> write('According to our database you are best suited to go for higher studies');true
  )
  )
  ).





% fail is to make sure remove_prev prev always returns true.
remove_prev:-
  retract(data(_,_)),
  fail.
remove_prev.

%:-dynamic(data/2);

start :-
  write('I am a basic career advisory system'), nl,
  write('To choose a answer input the index of the number followed by a dot (.)').

% =============
% Ask Questions
% =============


cgpa(Answer):-
  ask(gpa, Answer, [ten, nine, eight, seven, six, less_than_six]).

branch(Answer) :-
  ask(branch, Answer, [cse, csai, csd, csam, csss]).

papers(Answer) :-
  ask(research_papers_published, Answer, [one, two, greater_than_two]).

software_developing(Answer) :-
  ask(software_developing, Answer, [yes, no]).

machine_learning(Answer) :-
  ask(machine_learning, Answer, [yes, no]).

network(Answer) :-
  ask(network, Answer, [yes, no]).

iresearch(Answer) :-
  ask(iresearch, Answer, [yes, no]).

creativity(Answer) :-
  ask(creativity, Answer, [yes, no]).

logic(Answer) :-
  ask(logic, Answer, [yes, no]).

data_science(Answer) :-
  ask(data_science, Answer, [yes, no]).

research(Answer) :-
  ask(research, Answer, [yes, no]).

askall:-
  cgpa(X), branch(Y), machine_learning(Z), network(A), iresearch(B), creativity(C), logic(D), data_science(E), research(F).

% =========
% Questions
% =========

question(gpa):-
  write("Choose your CGPA"), nl.

question(branch):-
  write('Select your branch'), nl.

question(research_papers_published):-
  write('How many research papers have you published?'), nl.

question(software_developing):-
  write('Are you interested in building Applications?'), nl.

question(machine_learning):-
  write('Are you interested in Machine Learning and AI?'), nl.

question(data_science):-
  write('Are you interested in Data_science?'), nl.

question(network):-
  write('Are you interested in Computer Networks'), nl.

question(iresearch):-
  write('Do you like to deal with ideas, and problems when there is no clear answer?'), nl.

question(research):-
  write('Have you done any research in your field of interest?'), nl.

question(creativity):-
  write('Are you a creative person ?'), nl.

question(logic):-
  write('Are you a logical person ?'), nl.

% ========
% Answers
% ========

% ask their gpa

answer(one):-
  write(one).

answer(two):-
  write(two).

answer(greater_than_two):-
  write('Greater than two').

answer(cse):-
  write(cse).

answer(csai):-
  write(casi).

answer(csd):-
  write(csd).

answer(csam):-
  write(csam).

answer(csss):-
  write(csss).


answer(ten):-
  write('Ten').

answer(nine):-
  write('Nine').

answer(eight):-
  write('Eight').

answer(seven):-
  write('Seven').

answer(six):-
  write('Six').

answer(less_than_six):-
  write('< 6').

answer(yes) :-
  write('Yes.').

answer(no) :-
  write('No.').

% Outputs a nicely formatted list of answers
% [First|Rest] is the Choices list, Index is the index of First in Choices
answers([], _).
answers([First|Rest], Index) :-
  write(Index), write(' '), answer(First), nl,
  NextIndex is Index + 1,
  answers(Rest, NextIndex).

% Parses an Index and returns a Response representing the "Indexth" element in
% Choices (the [First|Rest] list)
parse(1, [First|_], First).
parse(Index, [First|Rest], Response) :-
  Index > 1,
  NextIndex is Index - 1,
  parse(NextIndex, Rest, Response).

% Asks the Question to the user and saves the Answer
ask(Question, Answer, Choices):-
  question(Question),
  answers(Choices, 1),
  read(Index),
  parse(Index, Choices, Response),
  asserta(data(Question, Response)),
  Response = Answer.


mydata([software_developing, data_scientist, it_consult, machine_learning, research]).

initialize:-
  nb_setval(software_developing, 0),
  nb_setval(data_scientist, 0),
  nb_setval(it_consult, 0),
  nb_setval(machine_learning, 0),
  nb_setval(research, 0).

getval(Index, Answer):-
  mydata(List),
  parse(Index, List, Response),
  nb_getval(Response, Answer).

addval(Index, Val):-
  mydata(List),
  parse(Index, List, Response),
  nb_getval(Response, X),
  NewVal is X + Val,
  nb_setval(Response, NewVal).


compute:-
  (
      data(branch, cse) -> (addval(1, 10))
    ; data(branch, casi) -> (addval(2, 10), addval(4, 20))
    ; addval(2, -5)
  ),
  (
      data(creativity, yes) -> (addval(1, 40), addval(2, 20), addval(3, 10), addval(4, 10), addval(5, 20))
    ; (addval(1, -20))
  ),
  (
      data(research, yes) -> (addval(1, 10), addval(2, 100), addval(3, 60), addval(4, 100), addval(5, 100))
    ; true
  ),
  (
      data(software_developing, yes) -> (addval(1, 40))
    ; addval(1, -20)
  ),
  (
      data(machine_learning, yes) -> (addval(4, 20))
    ; true
  ),
  (
      data(iresearch, yes) -> (addval(5, 100))
    ; addval(5, -50)
  ),
  (
      data(cgpa, ten) ->  addval(2, 20), addval(3, 20), addval(4, 20), addval(5, 40)
    ; data(cgpa, nine) ->  addval(2, 15), addval(3, 15), addval(4, 15), addval(5, 30)
    ; data(cgpa, eight) ->  addval(2, 10), addval(3, 10), addval(4, 10), addval(5, 20)
    ; data(cgpa, seven) ->  addval(2, 5), addval(3, 5), addval(4, 5), addval(5, -10)
    ; data(cgpa, six) ->  addval(5, -20)
    ; data(cgpa, ten) ->  addval(2, -5), addval(3, -5), addval(4, -5), addval(5, -40)
    ; true
  ),
  (
      data(logic, yes) -> (addval(1, 70), addval(2, 50), addval(3, 50), addval(4, 50), addval(5, 70))
    ; (addval(1, -20), addval(2, -30), addval(3, -20), addval(4, -50), addval(5, -50))
  ).

% Find the maximum number is the list

max_list([X|Xs],Max,Index):-
    max_list(Xs,X,0,0,Max,Index).

max_list([],OldMax,OldIndex,_, OldMax, OldIndex).
max_list([X|Xs],OldMax,_,CurrentIndex, Max, Index):-
    X > OldMax,
    NewCurrentIndex is CurrentIndex + 1,
    NewIndex is NewCurrentIndex,
    max_list(Xs, X, NewIndex, NewCurrentIndex, Max, Index).
max_list([X|Xs],OldMax,OldIndex,CurrentIndex, Max, Index):-
    X =< OldMax,
    NewCurrentIndex is CurrentIndex + 1,
    max_list(Xs, OldMax, OldIndex, NewCurrentIndex, Max, Index).


% create a list of values of jobs

create_list(1, [H|[]]):-
  getval(1, H).
create_list(Index, [H|T]):-
  getval(Index, H),
  Ni is Index - 1,
  Index > 1,
  create_list(Ni, T).
