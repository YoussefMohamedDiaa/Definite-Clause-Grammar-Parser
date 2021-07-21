 %handling s(T,[the,boy,pushed,quickly,a,box],[]).
 %handling  s(T,[the,boy,quickly,pushed,a,box],[]).
 %handling s(T,[the,old,boy,pushed,quickly,the,old,box],[]).
 %handling s(T,[the,old,boy,pushed,quickly,the,old,box,in,the,old,box],[]).
 %handling s(T,[the,old,boy,and,the,boy,pushed,quickly,the,old,box,in,the,old,box],[]).
 %handling  s(T,[the,boy,quickly,quickly,quickly,pushed,a,box],[]).
 %handling  s(T,[the,boy,pushed,quickly,quickly,quickly,a,box],[]).
 %handling s(T,[the,boy,pushed,quickly,a,box,in,a,box,in,the,box,in,a,box],[]).
 %handling s(T,[the,boy,pushed,and,pushed,quickly,a,box,in,a,box],[]).
 %handling s(T,[the,boy,pushed,quickly,a,box,in,a,box,and,pushed,quickly,a,box],[]).
 %handling s(T,[the,boy,pushed,a,box,and,the,boy,pushed,a,box],[]).
 %handling s(T,[the,boy,pushed,a,big,big,box],[]).
 %handling s(T,[the, old, woman, and, the, old, man, gave, the, poor, young, man, a, envelope],[]).
 %handling s(T,[the, young, boy, pushed, and, stored, a, big, box, in, the, room, after, school],[]).
 %handling s(T,[some, brilliant, students, and, many, professors, watched, and, admired, talented, lecturers],[]).
 %handling s(T,[the,boy,pushed],[]).
 %handling s(T,[who,quickly,climbed,the,tree],[]).
 %handling s(T,[what, did, professors, do],[]).
 
 s(s(K,U),A,Z) :- noun_phrase(K,A,B), verb_phrases(U,B,Z).
 s(s(K,U,Y,R),A,Z) :- noun_phrase(K,A,B), verb_phrases(U,B,M), conj(Y,M,N), s(R,N,Z).
 s(q(K,U),A,Z):- interrogativePronouns(K,A,B), verb_phrases(U,B,Z).
 s(q(K,U,Y),A,Z):- interrogativePronouns(K,A,B),what_verb(U,B,R),s(Y,R,Z).
 
 %with puncation
 s(s(K,U,Q),A,Z) :- noun_phrase(K,A,B), verb_phrases(U,B,F), punc(Q,F,Z).
 s(q(K,U,Q),A,Z):- interrogativePronouns(K,A,B), verb_phrases(U,B,F), punc(Q,F,Z).
 s(q(K,U,Y,Q),A,Z):- interrogativePronouns(K,A,B),what_verb(U,B,R),s(Y,R,F), punc(Q,F,Z).
 
 
 
 %pushed a box and pushed a box
 verb_phrases(U,B,Z):- verb_phrase(U,B,Z).
 verb_phrases(verb_phrases(U,Y,K),A,Z):- verb_phrase(U,A,B), conj(Y,B,M), verb_phrases(K,M,Z).
 
 % the old box
 noun_phrase(np(K,M,U),A,Z) :- det(K,A,B), adj_noun_phrase(M,U,B,Z).
 %the box
 noun_phrase(np(K,U),A,Z) :- det(K,A,B), noun(U,B,Z).
 %the box in
 noun_phrase(np(K,U,F,Y),A,Z) :- det(K,A,B), noun(U,B,R), prepo_noun_phrase(F,Y,R,Z).
 %the old box in 
 noun_phrase(np(K,M,U,F,Y),A,Z) :- det(K,A,B), adj_noun_phrase(M,U,B,R), prepo_noun_phrase(F,Y,R,Z). 
 %the box ands
 noun_phrase(np(K,U,F,Y),A,Z) :- det(K,A,B), noun(U,B,R), and_noun_phrase(F,Y,R,Z).
 %the old box ands 
 noun_phrase(np(K,M,U,F,Y),A,Z) :- det(K,A,B), adj_noun_phrase(M,U,B,R), and_noun_phrase(F,Y,R,Z).  


 % the nounpharses without det
 noun_phrase(np(M,U),B,Z) :-  adj_noun_phrase(M,U,B,Z).
 noun_phrase(np(U),B,Z) :-   noun(U,B,Z).
 noun_phrase(np(U,F,Y),B,Z) :-  noun(U,B,R), prepo_noun_phrase(F,Y,R,Z).
 noun_phrase(np(M,U,F,Y),B,Z) :-  adj_noun_phrase(M,U,B,R), prepo_noun_phrase(F,Y,R,Z). 
 noun_phrase(np(U,F,Y),B,Z) :-  noun(U,B,R), and_noun_phrase(F,Y,R,Z).
 noun_phrase(np(M,U,F,Y),B,Z) :-  adj_noun_phrase(M,U,B,R), and_noun_phrase(F,Y,R,Z).  

  
 
 verb_phrase(vp(K),A,B) :- verbs(K,A,B).
 %push noun
 verb_phrase(vp(K,U),A,Z) :- verbs(K,A,B), noun_phrase(U,B,Z).
  verb_phrase(vp(K,U,M),A,Z) :- verbs(K,A,B), noun_phrase(U,B,R), noun_phrase(M,R,Z).
 %quickly pushthe
 verb_phrase(vp(K,M,U),A,Z) :- adverb_verb_phrase(K,M,A,B), noun_phrase(U,B,Z).
 %push quickly noun
 verb_phrase(vp(K,M,U),A,Z) :- verb_adverb_phrase(K,M,A,B), noun_phrase(U,B,Z).
 
 
 adverb_verb_phrase(K,U,A,Z) :- advs(K,A,B), verbs(U,B,Z).
 verb_adverb_phrase(K,U,A,Z) :- verbs(K,A,B), advs(U,B,Z). 
 adj_noun_phrase(K,U,A,Z) :- adjs(K,A,B), noun(U,B,Z).
 %in noun_phrase
 prepo_noun_phrase(K,U,A,Z):- prepositions(K,A,R), noun_phrase(U,R,Z).
 %and noun_phrases
 and_noun_phrase(K,U,A,Z):- conj(K,A,R), noun_phrase(U,R,Z). 
 %and verbs
 verbs(K,A,B):- verb(K,A,B).
 verbs(verb_adv(K,F),A,Y):- verb(K,A,B), advs(F,B,Y).
 verbs(adv_verb(K,F),A,Y):- advs(K,A,B), verb(F,B,Y).
 verbs(andedVerbs(K,Z,O),A,Y):- verb(K,A,R), conj(Z,R,M), verbs(O,M,Y). 
 verbs(andedVerbs(F,K,Z,O),A,Y):- advs(F,A,B), verb(K,B,R), conj(Z,R,M), verbs(O,M,Y). 
 verbs(andedVerbs(K,F,Z,O),A,Y):- verb(K,A,B),advs(F,B,R), conj(Z,R,M), verbs(O,M,Y). 
 
 %adverbs aftereach others
 advs(K,A,B):-adv(K,A,B).
 advs(adverbs(K,U),A,Z):-adv(K,A,B), advs(U,B,Z).
 
 %adjs aftereach others
 adjs(K,A,B):-adj(K,A,B).
 adjs(adjectives(K,U),A,Z):-adj(K,A,B), adjs(U,B,Z). 
 
 
 %noun_phrases aftereach others 
 %noun_phrases(K,A,B):-  noun_phrase(K,A,B).
 %noun_phrases(noun_phrases(K,U),A,Z):- noun_phrase(K,A,B), noun_phrases(U,B,Z).
 
 
 noun(noun(people),[people|X], X). 
 noun(noun(family),[family|X], X). 
 noun(noun(government),[government|X], X). 
 noun(noun(history),[history|X], X). 
 noun(noun(health),[health|X], X). 
 noun(noun(art),[art|X], X). 
 noun(noun(researchers),[researchers|X], X). 
 noun(noun(girl),[girl|X], X).
 noun(noun(envelope),[envelope|X], X).
 noun(noun(boy),[boy|X], X). 
 noun(noun(box),[box|X], X). 
 noun(noun(room),[room|X], X). 
 noun(noun(school),[school|X], X). 
 noun(noun(woman),[woman|X], X). 
 noun(noun(man),[man|X], X). 
 %noun(noun(white),[white|X], X). 
 noun(noun(shed),[shed|X], X). 
 noun(noun(building),[building|X], X). 
 noun(noun(tree),[tree|X], X). 
 noun(noun(students),[students|X], X). 
 noun(noun(professors),[professors|X], X). 
 noun(noun(lecturers),[lecturers|X], X). 
 noun(noun(scientists),[scientists|X], X). 

 
 verb(verb(watched),[watched|X], X).
 verb(verb(pushed),[pushed|X], X). 
 verb(verb(stored),[stored|X], X). 
 verb(verb(gave),[gave|X], X). 
 verb(verb(climbed),[climbed|X], X). 
 verb(verb(watched),[watched|X], X). 
 verb(verb(admired),[admired|X], X). 
 verb(verb(appreciated),[appreciated|X], X). 
 verb(verb(did),[did|X], X).  
 verb(verb(do),[do|X], X). 
 verb(verb(accepted),[accepted|X], X). 
 verb(verb(added),[added|X], X). 
 verb(verb(admitted),[admitted|X], X).  
 verb(verb(announced),[announced|X], X).
 verb(verb(baked),[baked|X], X).
 verb(verb(behaved),[behaved|X], X).
 verb(verb(blinded),[blinded|X], X).
 verb(verb(blushed),[blushed|X], X).
 verb(verb(camped),[camped|X], X).
 verb(verb(collected),[collected|X], X).
 
 what_verb(what_verb(do),[do|X], X).
 what_verb(what_verb(did),[did|X], X).
 
 punc(full_stop(.),[.|X], X).
 punc(q_mark(?),[?|X], X).
 
 
 adj(adj(young),[young|X], X). 
 adj(adj(big),[big|X], X). 
 adj(adj(large),[large|X], X). 
 adj(adj(empty),[empty|X], X). 
 adj(adj(old),[old|X], X). 
 adj(adj(poor),[poor|X], X). 
 adj(adj(brilliant),[brilliant|X], X). 
 adj(adj(talented),[talented|X], X). 
 adj(adj(bright),[bright|X], X). 
 adj(adj(white),[white|X], X). 
 adj(adj(aggressive),[aggressive|X], X).
 adj(adj(brave),[brave|X], X).
 adj(adj(calm),[calm|X], X).
 adj(adj(delightful),[delightful|X], X). 
 adj(adj(eager),[eager|X], X).  
 adj(adj(gentle),[gentle|X], X).
 adj(adj(happy),[happy|X], X). 
 adj(adj(jolly),[jolly|X], X).
 adj(adj(kind),[kind|X], X).
 adj(adj(lively),[lively|X], X).
 adj(adj(nice),[nice|X], X). 
 adj(adj(obedient),[obedient|X], X). 
 adj(adj(polite),[polite|X], X).  
 
 adv(adv(boldly),[boldly|X], X). 
 adv(adv(bravely),[bravely|X], X). 
 adv(adv(cheerfully),[cheerfully|X], X). 
 adv(adv(deftly),[deftly|X], X).  
 adv(adv(devotedly),[devotedly|X], X).  
 adv(adv(eagerly),[eagerly|X], X). 
 adv(adv(elegantly),[elegantly|X], X). 
 adv(adv(gracefully),[gracefully|X], X). 
 adv(adv(happily),[happily|X], X). 
 adv(adv(honestly),[honestly|X], X). 
 adv(adv(kindly),[kindly|X], X). 
 adv(adv(quickly),[quickly|X], X).
 


 interrogativePronouns(interrogativePronouns(who),[who|X], X). 
 interrogativePronouns(interrogativePronouns(what),[what|X], X). 


 prepositions(prepositions(in),[in|X], X). 
 prepositions(prepositions(after),[after|X], X). 
 prepositions(prepositions(behind),[behind|X], X). 
 prepositions(prepositions(among),[among|X], X). 
 prepositions(prepositions(at),[at|X], X). 
 prepositions(prepositions(before),[before|X], X).  
 prepositions(prepositions(behind),[behind|X], X).  
 prepositions(prepositions(below),[below|X], X). 
 prepositions(prepositions(beneath),[beneath|X], X).  
 prepositions(prepositions(beside),[beside|X], X). 
 prepositions(prepositions(beyond),[beyond|X], X). 
 prepositions(prepositions(during),[during|X], X).  
 prepositions(prepositions(inside),[inside|X], X).  
 
 det(det(the),[the|X], X).
 det(det(an),[an|X], X). 
 det(det(a),[a|X], X). 
 det(det(many),[many|X], X).
 det(det(some),[some|X], X).
 det(det(every),[every|X], X).

 conj(conj(and),[and|X], X). 

