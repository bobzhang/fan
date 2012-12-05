open Format
{:transform| {"miniml" -> "dest" }
| $`int:i  -> $`int:i
| $`bool:b  ->  $`bool:b
| fst  $e ->  Pervasives.fst  $(rec e)
| snd $e ->  Pervasives.snd  $(rec e)
| $e1 + $e2 -> $(rec e1) + $(rec e2)
| $e1 - $e2 -> $(rec e1) - $(rec e2)
| $e1 * $e2 -> $(rec e1) * $(rec e2)
| $e1 = $e2 -> $(rec e1) = $(rec e2)  |}



















