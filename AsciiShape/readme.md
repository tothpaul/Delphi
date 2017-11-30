# AsciiShape

an ASCII Shape editor for Delphi Berlin

![screenshot](AsciiShapeEditor.png)

based on:
http://cocoamine.net/blog/2015/03/20/replacing-photoshop-with-nsstring/

another Delphi implementation (with the Pokeball shape)
https://github.com/Memnarch/AsciiImage

The main difference between this implementation and others is the ability to specify colors of the shapes.

after the shape you can define as much colors then the number of shapes in the picture.

Each line is made of 1 or 2 colors separated by a comma : $0,-1

the color is decimal or hexacimal with a "$" prefix (like in Delphi language)

the first color is the pen (stroke) color

the second color is the brush (fill) color

if the second color is "-1", then the shape is not filled

if the second color is not defined, it is equal to the first one.

to use the same colors for two consecutives shapes, you can put a blank line

the last defined colors applies to all the remaining shapes

An other difference with the original code is that the lines do not need to be all equals in lenght, the largest one define the final image width.
