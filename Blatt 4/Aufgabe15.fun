Allgemeine B¨aume k¨onnen in SML durch folgenden Datentyp dargestellt werden:
datatype ’a tree = Node of ’a * ’a tree list
a) Man implementiere in SML eine Funktion height, welche die H¨ohe eines allgemeinen
Baums berechnet. Die H¨ohe eines Baums ist die Anzahl der Knoten auf dem l¨angsten
Pfad im Baum.
b) Man implementiere in SML eine Funktion preorder, die eine Liste der Knotenwerte
in der Reihenfolge eines Durchlaufs in Pr¨aordnung ausgibt