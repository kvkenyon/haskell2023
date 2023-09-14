
Emp "BOSS" 12 [
    Emp "Jack" 2, Emp "Jane" 3, Emp "Alice" 4
]

We get something like this which shows BEST guest lists for each
employee for "BOSS".

The first element in each Pair is the BEST guest list if that employee
went to the party. For example, the first GL in the list "Jack" (in the first index) goes to the party, the second being the best guest list without "Jack" at the party.

We want two new guest lists
1. withBOSS
2. withoutBOSS

How do we get withBOSS?

If BOSS goes to the party, each employee he has will have 0 fun.

So we need to compute:

(glCons boss withoutEmp2 + glCons boss withoutEmp3 + ... ) -> the best list with boss without his 

To compute withoutBoss is the opposite 
withEmp2 + withEmp3 ... + withEmpN



[(GL withEmp2, GL withoutEmp2), (GL withEmp3, GL withoutEmp3), ...]

