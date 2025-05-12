To add (tentative)
- List.update_nth(x, 0, 5) //first arg: list, second arg: index, third arg: new element
- UDT.update(x, name, "Jack") //first arg: udt variable, second arg: field to update, third arg: new value for field



Dropped (confirmed)
- mut
- x[0] = 5 //Use List.update_nth(x, 0, 5) instead
- x.name = "Jack" Use UDT.update(x, name, "Jack") instead
- a += 5 //a is not mutable and this would not make any sense
- a -= 5 //a is not mutable and this would not make any sense
- a *= 5 //a is not mutable and this would not make any sense
- a /= 5 //a is not mutable and this would not make any sense
- a++ //a is not mutable and this would not make any sense
- a-- //a is not mutable and this would not make any sense
