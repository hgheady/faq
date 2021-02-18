# faq
In-memory logic programming interpreter of facts and queries

## Getting Started
Using [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade):
```
stack run < examples/$IN/in.txt
```

## Usage
### Facts
```
INPUT is_a_villain (Penguin)
INPUT is_a_villain (Dollmaker)
INPUT work_together (Bullock, Gordon)
INPUT work_together (Batman, Gordon)
INPUT work_together (Batman, Catwoman)
```

### Queries
#### Assertions
```
QUERY is_a_villain (Penguin)
---
true
```
```
QUERY is_a_villain (Venom)
---
false
```
Venom isn't anything (as far as we know)

#### Variables
 - All matching facts are returned.

```
QUERY work_together (X, Gordon)
---
X: Batman
X: Bullock
```

```
QUERY is_a_villain (X)
---
X: Penguin
X: Dollmaker
```

```
QUERY work_together (X, Y)
---
X: Bullock, Y: Gordon
X: Batman, Y: Gordon
X: Batman, Y: Catwoman
```

- Variables stand for the same thing if they appear multiple times.

```
INPUT work_together (Two-Face, Two-Face)
QUERY work_together (X, X)
---
X: Two-Face
```
