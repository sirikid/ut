-module(ut_tests).

-include_lib("eunit/include/eunit.hrl").

expand_test_() ->
    Substitutes =
        #{
          count => [<<"one">>, <<"two">>, <<"three">>],
          dom => [<<"example">>, <<"com">>],
          dub => <<"me/too">>,
          hello => <<"Hello World!">>,
          half => <<"50%">>,
          var => <<"value">>,
          who => <<"fred">>,
          base => <<"http://example.com/home/">>,
          path => <<"/foo/bar">>,
          list => [<<"red">>, <<"green">>, <<"blue">>],
          keys => #{<<"semi">> => <<";">>, <<"dot">> => <<".">>, <<"comma">> => <<",">>},
          v => <<"6">>,
          x => <<"1024">>,
          y => <<"768">>,
          empty => <<>>,
          empty_keys => #{}
          %% undef => null
         },
    [{Title ++ Template,
      ?_assertEqual(Expansion, ut:expand(Template, Substitutes, [{keys, atoms}]))}
     || {Title, Cases} <- groups(), {Template, Expansion} <- Cases].

groups() ->
    [{"Simple String Expansion: ",
      [{"{var}", <<"value">>},
       {"{hello}", <<"Hello%20World%21">>},
       {"{half}", <<"50%25">>},
       {"O{empty}X", <<"OX">>},
       {"O{undef}X", <<"OX">>},
       {"{x,y}", <<"1024,768">>},
       {"{x,hello,y}", <<"1024,Hello%20World%21,768">>},
       {"?{x,empty}", <<"?1024,">>},
       {"?{x,undef}", <<"?1024">>},
       {"?{undef,y}", <<"?768">>},
       {"{var:3}", <<"val">>},
       {"{var:30}", <<"value">>},
       {"{list}", <<"red,green,blue">>},
       {"{list*}", <<"red,green,blue">>},
       %% {"{keys}", <<"semi,%3B,dot,.,comma,%2C">>},
       %% {"{keys*}", <<"semi=%3B,dot=.,comma=%2C">>},
       {"{keys}", <<"comma,%2C,dot,.,semi,%3B">>},
       {"{keys*}", <<"comma=%2C,dot=.,semi=%3B">>}]},
     {"Reserved Expansion: ",
      [{"{+var}", <<"value">>},
       {"{+hello}", <<"Hello%20World!">>},
       {"{+half}", <<"50%25">>},

       {"{base}index", <<"http%3A%2F%2Fexample.com%2Fhome%2Findex">>},
       {"{+base}index", <<"http://example.com/home/index">>},
       {"O{+empty}X", <<"OX">>},
       {"O{+undef}X", <<"OX">>},

       {"{+path}/here", <<"/foo/bar/here">>},
       {"here?ref={+path}", <<"here?ref=/foo/bar">>},
       {"up{+path}{var}/here", <<"up/foo/barvalue/here">>},
       {"{+x,hello,y}", <<"1024,Hello%20World!,768">>},
       {"{+path,x}/here", <<"/foo/bar,1024/here">>},

       {"{+path:6}/here", <<"/foo/b/here">>},
       {"{+list}", <<"red,green,blue">>},
       {"{+list*}", <<"red,green,blue">>},
       %% {"{+keys}", <<"semi,%3B,dot,.,comma,%2C">>},
       %% {"{+keys*}", <<"semi=%3B,dot=.,comma=%2C">>},
       {"{+keys}", <<"comma,,,dot,.,semi,;">>},
       {"{+keys*}", <<"comma=,,dot=.,semi=;">>}]},
     {"Fragment Expansion: ",
      [{"{#var}",         <<"#value">>},
       {"{#hello}",       <<"#Hello%20World!">>},
       {"{#half}",        <<"#50%25">>},
       {"foo{#empty}",    <<"foo#">>},
       {"foo{#undef}",    <<"foo">>},
       {"{#x,hello,y}",   <<"#1024,Hello%20World!,768">>},
       {"{#path,x}/here", <<"#/foo/bar,1024/here">>},
       {"{#path:6}/here", <<"#/foo/b/here">>},
       {"{#list}",        <<"#red,green,blue">>},
       {"{#list*}",       <<"#red,green,blue">>},
       %% {"{#keys}",        <<"#semi,;,dot,.,comma,,">>},
       %% {"{#keys*}",       <<"#semi=;,dot=.,comma=,">>},
       {"{#keys}",        <<"#comma,,,dot,.,semi,;">>},
       {"{#keys*}",       <<"#comma=,,dot=.,semi=;">>}]},
     {"Label Expansion with Dot-Prefix: ",
      [{"{.who}",           <<".fred">>},
       {"{.who,who}",       <<".fred.fred">>},
       {"{.half,who}",      <<".50%25.fred">>},
       {"www{.dom*}",       <<"www.example.com">>},
       {"X{.var}",          <<"X.value">>},
       {"X{.empty}",        <<"X.">>},
       {"X{.undef}",        <<"X">>},
       {"X{.var:3}",        <<"X.val">>},
       {"X{.list}",         <<"X.red,green,blue">>},
       {"X{.list*}",        <<"X.red.green.blue">>},
       %% {"X{.keys}",         <<"X.semi,%3B,dot,.,comma,%2C">>},
       %% {"X{.keys*}",        <<"X.semi=%3B.dot=..comma=%2C">>},
       {"X{.keys}",         <<"X.comma,%2C,dot,.,semi,%3B">>},
       {"X{.keys*}",        <<"X.comma=%2C.dot=..semi=%3B">>},
       {"X{.empty_keys}",   <<"X">>},
       {"X{.empty_keys*}",  <<"X">>}]},
     {"Path Segment Expansion: ",
      [{"{/who}",           <<"/fred">>},
       {"{/who,who}",       <<"/fred/fred">>},
       {"{/half,who}",      <<"/50%25/fred">>},
       {"{/who,dub}",       <<"/fred/me%2Ftoo">>},
       {"{/var}",           <<"/value">>},
       {"{/var,empty}",     <<"/value/">>},
       {"{/var,undef}",     <<"/value">>},
       {"{/var,x}/here",    <<"/value/1024/here">>},
       {"{/var:1,var}",     <<"/v/value">>},
       {"{/list}",          <<"/red,green,blue">>},
       {"{/list*}",         <<"/red/green/blue">>},
       {"{/list*,path:4}",  <<"/red/green/blue/%2Ffoo">>},
       %% {"{/keys}",          <<"/semi,%3B,dot,.,comma,%2C">>},
       %% {"{/keys*}",         <<"/semi=%3B/dot=./comma=%2C">>},
       {"{/keys}",          <<"/comma,%2C,dot,.,semi,%3B">>},
       {"{/keys*}",         <<"/comma=%2C/dot=./semi=%3B">>}
      ]},
     {"Path-Style Parameter Expansion: ",
      [{"{;who}",          <<";who=fred">>},
       {"{;half}",         <<";half=50%25">>},
       {"{;empty}",        <<";empty">>},
       {"{;v,empty,who}",  <<";v=6;empty;who=fred">>},
       {"{;v,bar,who}",    <<";v=6;who=fred">>},
       {"{;x,y}",          <<";x=1024;y=768">>},
       {"{;x,y,empty}",    <<";x=1024;y=768;empty">>},
       {"{;x,y,undef}",    <<";x=1024;y=768">>},
       {"{;hello:5}",      <<";hello=Hello">>},
       {"{;list}",         <<";list=red,green,blue">>},
       {"{;list*}",        <<";list=red;list=green;list=blue">>},
       %% {"{;keys}",         <<";keys=semi,%3B,dot,.,comma,%2C">>},
       %% {"{;keys*}",        <<";semi=%3B;dot=.;comma=%2C">>},
       {"{;keys}",         <<";keys=comma,%2C,dot,.,semi,%3B">>},
       {"{;keys*}",        <<";comma=%2C;dot=.;semi=%3B">>}]},
     {"Form-Style Query Expansion: ",
      [{"{?who}",        <<"?who=fred">>},
       {"{?half}",       <<"?half=50%25">>},
       {"{?x,y}",        <<"?x=1024&y=768">>},
       {"{?x,y,empty}",  <<"?x=1024&y=768&empty=">>},
       {"{?x,y,undef}",  <<"?x=1024&y=768">>},
       {"{?var:3}",      <<"?var=val">>},
       {"{?list}",       <<"?list=red,green,blue">>},
       {"{?list*}",      <<"?list=red&list=green&list=blue">>},
       %% {"{?keys}",       <<"?keys=semi,%3B,dot,.,comma,%2C">>},
       %% {"{?keys*}",      <<"?semi=%3B&dot=.&comma=%2C">>},
       {"{?keys}",       <<"?keys=comma,%2C,dot,.,semi,%3B">>},
       {"{?keys*}",      <<"?comma=%2C&dot=.&semi=%3B">>}]},
     {"Form-Style Query Continuation: ",
      [{"{&who}",          <<"&who=fred">>},
       {"{&half}",         <<"&half=50%25">>},
       {"?fixed=yes{&x}",  <<"?fixed=yes&x=1024">>},
       {"{&x,y,empty}",    <<"&x=1024&y=768&empty=">>},
       {"{&x,y,undef}",    <<"&x=1024&y=768">>},
       {"{&var:3}",        <<"&var=val">>},
       {"{&list}",         <<"&list=red,green,blue">>},
       {"{&list*}",        <<"&list=red&list=green&list=blue">>},
       %% {"{&keys}",         <<"&keys=semi,%3B,dot,.,comma,%2C">>},
       %% {"{&keys*}",        <<"&semi=%3B&dot=.&comma=%2C">>},
       {"{&keys}",         <<"&keys=comma,%2C,dot,.,semi,%3B">>},
       {"{&keys*}",        <<"&comma=%2C&dot=.&semi=%3B">>}]}].

more_expand_test() ->
    ?assertEqual(
       <<"https://acme-news.com/posts/2023/9/4/Breaking%21">>,
       ut:expand(
         "https://acme-news.com{/section,date.year,date.month,date.day,title:10}",
         #{section => posts, title => <<"Breaking!">>, date => #{year => 2023, month => 9, day => 4}},
         [{keys, atoms}]
        )
      ).