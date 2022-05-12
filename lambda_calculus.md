
$(\lambda xy.xxy)(\lambda x.xy)(\lambda x.xz)$

$(\lambda x(\lambda y.xxy))(\lambda x.xy)(\lambda x.xz)$

$[x:= (\lambda x.xy)]$

$(\lambda y.(\lambda x.xy)(\lambda x.xy)y)(\lambda x.xz)$

$[y:= (\lambda x.xz)]$

$(\lambda x.xy)(\lambda x.xy)(\lambda x.xz)$

$[x:= (\lambda x.xy)]$

$(\lambda x.xy)y(\lambda x.xz)$

$[x:= y]$

$yy(\lambda x.xz)$

Lambda function beta reduction exercise:
$(\lambda xyz.xz(yz))(\lambda mn.m)(\lambda p.p)$
$(\lambda yz.(\lambda mn.m)z(yz))(\lambda p.p)$
$(\lambda z.(\lambda mn.m)z(\lambda p.pz))$
$(\lambda z.(\lambda n.z)(\lambda p.pz))$
$(\lambda z.z)$


$\text{1.11 Exercise}$

Combinators
1. yes
2. no - z is a free variable
3. yes
4. yes
5. no

Normal form or divergent?
1. beta normal form - because no free variables
2. divergent 
3. can be reduced to beta normal form

$\text{Beta reducuction exercises using normal order evaluation strategy}$

1. $(\lambda abc.cba)zz(\lambda wv.w)$
   1. $[a:= z]$
   2. $(\lambda bc.cbz)(z)(\lambda wv.w)$
   3. $[b:= z]$
   4. $(\lambda c.czz)(\lambda wv.w)$
   5. $[c:= (\lambda wv.w)]$
   6. $(\lambda wv.w)zz$
   7. $[w:= z]$
   8. $(\lambda v.z)z$
   9. $[v:= z]$
   10. $z$

2. $(\lambda x.\lambda y.xyy)(\lambda a.a)b$
   1. $[x:= (\lambda a.a)]$
   2. $(\lambda y.(\lambda a.a)yy)b$
   3. $[y:= b]$
   4. $(\lambda a.a)bb$
   5. $[a: =b]$
   6. $bb$

3. $(\lambda y.y)(\lambda x.xx)(\lambda z.zq)$
   1. $[y:= (\lambda x.xx)]$
   2. $(\lambda x.xx)(\lambda z.zq)$
   3. $[x:= (\lambda z.zq)]$
   4. $(\lambda z.zq)(\lambda z.zq)$
   5. $[z:= (\lambda z.zq)]$
   6. $(\lambda z .zq)q$
   7. $[z:= q]$
   8. $qq$

4. $(\lambda z.z)(\lambda z.zz)(\lambda z.zy)$
   1. $[z:= (\lambda z.zz)]$
   2. $(\lambda z.zz)(\lambda z.zy)$
   3. $[z:= (\lambda z.zy)]$
   4. $(\lambda z.zy)(\lambda z.zy)$
   5. $[z:= (\lambda z.zy)]$
   6. $(\lambda z.zy)y$
   7. $[z:= y]$
   8. $yy$

5. $(\lambda x.\lambda y.xyy)(\lambda y.y)y$
   1. $[x:= (\lambda y.y)]$
   2. $(\lambda y.(\lambda y.y)yy)y$
   3. $[y:= y]$
   4. $(\lambda y.y)yy$
   5. $[y:= y]$
   6. $yy$

6. $(\lambda a.aa)(\lambda b.ba)c$
   1. $[a:= (\lambda b.ba)]$
   2. $(\lambda b.ba)(\lambda b.ba)c$
   3. $[b:= (\lambda b.ba)]$
   4. $(\lambda b.ba)ac$
   5. $[b:= a]$
   6. $aac$

7. $(\lambda xyz.xz(yz))(\lambda x.z))(\lambda x.a)$

- In what follows, we use '$z1$' to distinguish the free variable '$z$' from the bounded variable of the same name.
   1. $[x:= (\lambda x.z)]$
   2. $(\lambda yz.(\lambda x.z_1)z(yz))(\lambda x.a)$
   3. $[y:= (\lambda x.a)]$
   4. $(\lambda z.(\lambda x.z_1)z((\lambda x.a)z))$
   5. $[x:= z]$
   6. $(\lambda z.z_1((\lambda x.a)z))$
   7. $[x:= z]$
   8. $(\lambda z.z_1a)$


Reducible expressions are also known as redexes
