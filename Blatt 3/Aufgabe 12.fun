(*
Quadbäume sind eine Baum-Erweiterung (siehe Folie 59) mit folgenden zusätzlichen Eigenschaften:
Jeder Knoten hat genau vier Kinder oder gar keine
Falls ein Knoten keine Kinder hat: Jedem Knoten wird maximal ein Element zugeordnet, gültige Zuordnungen sind: Rechteck (coordinate, height, width), Kreis(Centre, radius), nichts
Als Knotenindizierung gilt eine zweidimensionale Koordinate 


Der PSeudocode für so eine Struktur sieht folgendermaßen aus:

beginne bei koordinate (x,y)
fun construct QuadTree(x,y):
    bilde vier Quadrate: (<x,<y); (<x, >y);(>x, <y); (>x,>y)
    für jedes Quadrat: 
        if in diesem Quadrat kein Objekt oder genau ein Objekt liegt: finish. 
        Else: setze (x1,y2) in die Mitte zwischen zwei Objekten, constructQuadTree(x1,y2)



*)