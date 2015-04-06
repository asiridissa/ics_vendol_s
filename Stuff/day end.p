select max(r.bill#),sum(r.pieses)
from recipts r
group by r.bill#
