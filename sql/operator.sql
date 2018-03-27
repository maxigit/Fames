insert into fames_operator
(nickname
, firstname
, surname
, active)
select name as nickname
    , firstname
     , surname
     , active
     from mop.operator
     where active = 1
     and name not in
(select nickname from fames_operator)

