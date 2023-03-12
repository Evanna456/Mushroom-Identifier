%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%poisonous mushroom identification system
main:-identify.
identify:-
  retractall(known(_,_,_)),         % clear stored information
  mushroom(X),
  write('The mushroom is a '),write(X),nl.
identify:-
  write('I can''t identify that mushroom, Not Poisonous'),nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

order("agaricales"):-
  gills(has).


family("amanitaceae"):-
  order("agaricales"),
  gillstype(free),
  gillscolor(white),
  sporeprint(white).

family("cortinariaceae"):-
  order("agaricales").

%%%%%%%%%%%%%%%%%%%%%% New Added Mushroom Family
family("agaricaceae"):-
  order("agaricales").

family("psathyrellaceae"):-
  order("agaricales").

family("entolomataceae"):-
  order("agaricales").

family("hymenogastraceae"):-
  order("agaricales").

mushroom("cortinarius_gentilis"):-
  family("cortinariaceae"),
  distribution(northern_america),
  capshape(convex),
  capcolor(brown),
  gillstype(adnexed),
  gillscolor(brown),
  stipe(has),
  stipetype(cortina),
  stipecolor(whitish_brown),
  sporeprint(reddish_brown).

%%%%%%%%%%%%%%%%%%%%%%%%%%% New Added Mushroom
mushroom("california_agaricus"):-
  family("agaricaceae"),
  distribution(northern_america),
  capshape(convex),
  capcolor(white),
  gillstype(free),
  gillscolor(brown),
  stipe(has),
  stipetype(ring),
  stipecolor(white),
  sporeprint(brown).

mushroom("felt_ringed_agaricus"):-
  family("agaricaceae"),
  distribution(northern_america),
  capshape(convex),
  capcolor(white),
  gillstype(free),
  gillscolor(white),
  stipe(has),
  stipetype(ring),
  stipecolor(white),
  sporeprint(brown_to_purple_brown).

mushroom("yellow_staining_mushroom"):-
  family("agaricaceae"),
  distribution(europe),
  capshape(convex),
  capcolor(white),
  gillstype(free),
  gillscolor(white),
  stipe(has),
  stipetype(ring),
  stipecolor(white),
  sporeprint(brown).

mushroom("sunshine_amanita"):-
  family("agaricaceae"),
  distribution(northern_america),
  capshape(flat),
  capcolor(yellowish),
  gillstype(free),
  gillscolor(white),
  stipe(has),
  stipetype(ring_and_volva),
  stipecolor(white),
  sporeprint(white).

mushroom("brown_american_star_footed_amanita"):-
  family("amanitaceae"),
  distribution(northern_america),
  capshape(flat),
  capcolor(brownish),
  gillstype(free),
  gillscolor(white),
  stipe(has),
  stipetype(ring_and_volva),
  stipecolor(white),
  sporeprint(white). 

mushroom("cokers_amanita"):-
  family("amanitaceae"),
  distribution(northern_america),
  capshape(convex),
  capcolor(white),
  gillstype(free),
  gillscolor(white),
  stipe(has),
  stipetype(ring_and_volva),
  stipecolor(white),
  sporeprint(white).

mushroom("european_solitary_amanita"):-
  family("amanitaceae"),
  distribution(europe),
  capshape(convex),
  capcolor(white_to_ivory),
  gillstype(free),
  gillscolor(white),
  stipe(has),
  stipetype(ring),
  stipecolor(white_to_ivory),
  sporeprint(white).

mushroom("powdery_amanita"):-
  family("amanitaceae"),
  distribution(northern_america),
  capshape(flat_or_convex),
  capcolor(grayish),
  gillstype(free),
  gillscolor(white),
  stipe(has),
  stipetype(bare),
  stipecolor(white),
  sporeprint(white).

mushroom("gemmed_amanita"):-
  family("amanitaceae"),
  distribution(northern_africa),
  capshape(flat_or_convex),
  capcolor(yellowish),
  gillstype(adnate),
  gillscolor(white),
  stipe(has),
  stipetype(ring),
  stipecolor(white),
  sporeprint(white).

mushroom("fly_amanita"):-
  family("amanitaceae"),
  distribution(world_wide),
  capshape(flat_or_convex),
  capcolor(red_orange),
  gillstype(free),
  gillscolor(white),
  stipe(has),
  stipetype(ring_and_volva),
  stipecolor(white),
  sporeprint(white).

mushroom("panther_cap"):-
  family("amanitaceae"),
  distribution(europe),
  capshape(flat),
  capcolor(brownish),
  gillstype(free),
  gillscolor(white),
  stipe(has),
  stipetype(ring_and_volva),
  stipecolor(white),
  sporeprint(white).

mushroom("grey_vieled_amanita"):-
  family("amanitaceae"),
  distribution(northern_america),
  capshape(convex),
  capcolor(brown),
  gillstype(free),
  gillscolor(white),
  stipe(has),
  stipetype(ring_and_volva),
  stipecolor(white),
  sporeprint(white).

mushroom("smiths_amanita"):-
  family("amanitaceae"),
  distribution(northern_america),
  capshape(convex),
  capcolor(white),
  gillstype(free),
  gillscolor(white),
  stipe(has),
  stipetype(ring_and_volva),
  stipecolor(white),
  sporeprint(white).

mushroom("green_spored_parasol"):-
  family("agarcaceae"),
  distribution(world_wide),
  capshape(flat),
  capcolor(white),
  gillstype(free),
  gillscolor(white),
  stipe(has),
  stipetype(ring),
  stipecolor(white),
  sporeprint(green).

mushroom("common_ink_cap"):-
  family("psathyrellaceae"),
  distribution(australia),
  capshape(ovate),
  capcolor(white),
  gillstype(free),
  gillscolor(white),
  stipe(has),
  stipetype(bare),
  stipecolor(white),
  sporeprint(black).

mushroom("cinnamon_webcap"):-
  family("cortinariaceae"),
  distribution(northern_america),
  capshape(convex),
  capcolor(brown),
  gillstype(emarginate),
  gillscolor(brown),
  stipe(has),
  stipetype(bare),
  stipecolor(brown),
  sporeprint(NA).

mushroom("cortinarius_gentilis"):-
  family("cortinariaceae"),
  distribution(northern_america),
  capshape(convex),
  capcolor(brown),
  gillstype(adnexed),
  gillscolor(brown),
  stipe(has),
  stipetype(cortina),
  stipecolor(brown),
  sporeprint(reddish_brown).

mushroom("freckled_dappering"):-
  family("agaricaceae"),
  distribution(oceania),
  capshape(campanulate),
  capcolor(brown),
  gillstype(free),
  gillscolor(brown),
  stipe(has),
  stipetype(ring),
  stipecolor(brown),
  sporeprint(white).

mushroom("wood_pinkgill"):-
  family("entolomataceae"),
  distribution(northern_america),
  capshape(convex),
  capcolor(white),
  gillstype(adnate),
  gillscolor(white),
  stipe(has),
  stipetype(bare),
  stipecolor(white),
  sporeprint(pink).

mushroom("livid_entomola"):-
  family("entolomataceae"),
  distribution(asia),
  capshape(convex),
  capcolor(white),
  gillstype(adnate),
  gillscolor(white),
  stipe(has),
  stipetype(bare),
  stipecolor(white),
  sporeprint(pink).
  
mushroom("poison_pie"):-
  family("hymenogastraceae"),
  distribution(europe),
  capshape(umbonate),
  capcolor(white),
  gillstype(adnate),
  gillscolor(white),
  stipe(has),
  stipetype(bare),
  stipecolor(white),
  sporeprint(brown).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

distribution(X):- menuask(distribution,X,["northern_america","asia","europe","northern_africa","australia","world_wide"]).

capshape(X):- menuask(capshape,X,["convex","flat","flat_or_convex","ovate","campanulate","umbonate"]).

capcolor(X):- menuask(capcolor,X,["white","brown", "whitish_brown","brownish","white_to_ivory","grayish","red_orange"]).

gills(X):- ask(gills,X).

gillstype(X):- menuask(gillstype,X,["adnexed","free","adnate","emarginate"]).

gillscolor(X):- menuask(gillscolor,X,["white","brown","red"]).

stipe(X):- ask(stipe,X).

stipetype(X):- menuask(stipetype,X,["ring","volva","ring_and_volva","cortina","bare"]).

stipecolor(X):- menuask(stipecolor,X,["white","brown","white_to_ivory"]).

sporeprint(X):- menuask(sporeprint,X,["white","reddish_brown","brown","brown_to_purple_brown","green","black","NA"]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%ask engine

% "ask" is responsible for getting information from the user, and remembering
% the users response. If it doesn't already know the answer to a question
% it will ask the user. It then asserts the answer. It recognizes two
% cases of knowledge: 1) the attribute-value is known to be true,
% 2) the attribute-value is known to be false.

% This means an attribute might have multiple values. A third test to
% see if the attribute has another value could be used to enforce
% single valued attributes. (This test is commented out below)

% For this system the menuask is used for attributes which are single
% valued

% "ask" only deals with simple yes or no answers. a "yes" is the only
% yes value. any other response is considered a "no".

ask(Attribute,Value):-
  known(yes,Attribute,Value),       % succeed if we know its true
  !.                                % and dont look any further
ask(Attribute,Value):-
  known(_,Attribute,Value),         % fail if we know its false
  !, fail.

ask(Attribute,_):-
  known(yes,Attribute,_),           % fail if we know its some other value.
  !, fail.                          % the cut in clause #1 ensures that if
                                    % we get here the value is wrong.
ask(A,V):-
  write(A:V),                       % if we get here, we need to ask.
  write('? (yes or no): '),
  read(Y),                          % get the answer
  asserta(known(Y,A,V)),            % remember it so we dont ask again.
  Y = yes.                          % succeed or fail based on answer.

% "menuask" is like ask, only it gives the user a menu to to choose
% from rather than a yes on no answer. In this case there is no
% need to check for a negative since "menuask" ensures there will
% be some positive answer.

menuask(Attribute,Value,_):-
  known(yes,Attribute,Value),       % succeed if we know
  !.
menuask(Attribute,_,_):-
  known(yes,Attribute,_),           % fail if its some other value
  !, fail.

menuask(Attribute,AskValue,Menu):-
  nl,write('What is the value for '),write(Attribute),write(' the mushroom?'),nl,
  display_menu(Menu),
  write('Enter the number of choice> '),
  read(Num),nl,
  pick_menu(Num,AnswerValue,Menu),
  asserta(known(yes,Attribute,AnswerValue)),
  AskValue = AnswerValue.           % succeed or fail based on answer

display_menu(Menu):-
  disp_menu(1,Menu), !.             % make sure we fail on backtracking

disp_menu(_,[]).
disp_menu(N,[Item | Rest]):-        % recursively write the head of
  write(N),write(' : '),write(Item),nl, % the list and disp_menu the tail
  NN is N + 1,
  disp_menu(NN,Rest).

pick_menu(N,Val,Menu):-
  integer(N),                       % make sure they gave a number
  pic_menu(1,N,Val,Menu), !.        % start at one
  pick_menu(Val,Val,_).             % if they didn't enter a number, use
                                    % what they entered as the value

pic_menu(_,_,none_of_the_above,[]). % if we've exhausted the list
pic_menu(N,N, Item, [Item|_]).      % the counter matches the number
pic_menu(Ctr,N, Val, [_|Rest]):-
  NextCtr is Ctr + 1,               % try the next one
  pic_menu(NextCtr, N, Val, Rest).