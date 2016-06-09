translate(a0,0).
translate(a1,6).
translate(a2,12).
translate(a3,18).
translate(a4,24).
translate(a5,30).

translate(b0,1).
translate(b1,7).
translate(b2,13).
translate(b3,19).
translate(b4,25).
translate(b5,31).

translate(c0,2).
translate(c1,8).
translate(c2,14).
translate(c3,20).
translate(c4,26).
translate(c5,32).

translate(d0,3).
translate(d1,9).
translate(d2,15).
translate(d3,21).
translate(d4,27).
translate(d5,33).

translate(e0,4).
translate(e1,10).
translate(e2,16).
translate(e3,22).
translate(e4,28).
translate(e5,34).

translate(f0,5).
translate(f1,11).
translate(f2,17).
translate(f3,23).
translate(f4,29).
translate(f5,35).

translate(44,44).

translate(s1,[2,3,1,2,2,3,2,1,3,1,3,1,1,3,2,3,1,2,3,1,2,1,3,2,2,3,1,3,1,3,2,1,3,2,2,1]).
translate(s2,[1,2,2,3,1,2,3,1,3,1,3,2,2,3,1,2,1,3,2,1,3,2,3,1,1,3,1,3,1,2,3,2,2,1,3,2]).
translate(s3,[2,2,3,1,2,2,1,3,1,3,1,3,3,1,2,2,3,1,2,3,1,3,1,2,2,1,3,1,3,2,1,3,2,2,1,3]).
translate(s4,[3,1,2,2,3,1,2,3,1,3,1,2,2,1,3,1,3,2,1,3,2,2,1,3,3,1,3,1,3,1,2,2,1,3,2,2]).

% In generateHint, translate is used in the opposite way(machine readable->humain readable),
% to avoid 2 can be either easy or c0, it is necessary to use another name for difficultity tranlation.
translateD(easy,2).
translateD(normal,3).
translateD(hard,4).
translateD(challenge,5).
