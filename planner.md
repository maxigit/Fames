# 
shelves to boxes
   boxes in shelves
     < E0 
     in E0


boxes to shelves
   shelves containting boxes 
      > B1 ?
      has B1
      
sibling, boxes in same shelves as ...
  combination of contains and in ?
    - containing(B1).content
    - B1.shelves.boxes
    - B1#@
    - B1<>
    - <B1>
    - <(>B1)
    - [([B1])]
    - in (has B1))
    - B1$#
    - B1#*

  


shelves to position 
   - split by box
   - split by formula, offset
   
temporary tags ?


Current selection, of boxes , of shelvel ?

# Filtering
[...]
or [..] for boxes
   <..> for shelves
   @..@ ---
   (..) for shelves
## tags
- [tag]
- [!tag]
- [tag=value]
## quantities
- [1..2] 2 firsts
-  [-1] lastn one
## shelf or box ?

  B1[tag=value] B1 with tag=value
  B1<tag=value> B1 on shelf with tag=value
  
  B1<[tag=value]
  B1 in [tag=value]
  E* with [tag/value]


# All
All boxees all shelves
- * #
  [*] (*)
  boxes shelves
  B S
  $B %
  * /*/
  # $

# Sorting
- R way with external [order]
     shelves[ shelves^ .shelfname]
- operator list ^ with
- sort by 
# Grouping with inside operatio
see list as 2 (n) dimension ?
     boxes[ ,1]
     
# Current set of boxes (shelves)     
are consuming when moved
## pileplie
  B1 > S1 | #moved
  > S2 <!-- left over -->
