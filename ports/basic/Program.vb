Imports System
Imports System.IO
Imports System.Text
Imports System.Text.Json
Imports System.Text.RegularExpressions
Imports System.Collections.Generic

Module Program

    ' =========================
    ' Symtab
    ' =========================
    Public Class Symtab
        Private ReadOnly map As New Dictionary(Of String, Integer)(StringComparer.Ordinal)
        Private ReadOnly vec As New List(Of String)

        Public Function Intern(s As String) As Integer
            Dim id As Integer
            If map.TryGetValue(s, id) Then Return id
            id = vec.Count
            vec.Add(s)
            map(s) = id
            Return id
        End Function

        Public Function Str(id As Integer) As String
            If id >= 0 AndAlso id < vec.Count Then Return vec(id)
            Return "?"
        End Function

        Public Function IsVar(id As Integer) As Boolean
            Dim s = Str(id)
            Return s.Length > 0 AndAlso s(0) = "?"c
        End Function
    End Class

    ' =========================
    ' Feats
    ' =========================
    Public Structure Feat
        Public K As Integer
        Public V As Integer
        Public Sub New(k As Integer, v As Integer)
            Me.K = k : Me.V = v
        End Sub
    End Structure

    Public Module Feats
        Private Const FNV_OFF As ULong = 1469598103934665603UL
        Private Const FNV_PR As ULong = 1099511628211UL

        Public Function Norm(fs As List(Of Feat)) As List(Of Feat)
            fs.Sort(Function(a, b)
                        If a.K <> b.K Then Return a.K.CompareTo(b.K)
                        Return a.V.CompareTo(b.V)
                    End Function)
            Return fs
        End Function

        Public Function Find(fs As List(Of Feat), key As Integer) As Integer?
            For Each f In fs
                If f.K = key Then Return f.V
            Next
            Return Nothing
        End Function

        Public Function Hash64(fs As List(Of Feat)) As ULong
            Dim h As ULong = FNV_OFF
            For Each f In fs
                h = (h Xor CULng(f.K)) * FNV_PR
                h = (h Xor CULng(f.V)) * FNV_PR
            Next
            Return h
        End Function

        Public Function ReplaceValue(fs As List(Of Feat), fromV As Integer, toV As Integer) As List(Of Feat)
            Dim out As New List(Of Feat)(fs.Count)
            For Each f In fs
                Dim v2 = If(f.V = fromV, toV, f.V)
                out.Add(New Feat(f.K, v2))
            Next
            Return Norm(out)
        End Function

        Public Function Unify(st As Symtab, a As List(Of Feat), b As List(Of Feat)) As List(Of Feat)
            Dim mp As New Dictionary(Of Integer, Integer)
            For Each f In a
                mp(f.K) = f.V
            Next
            For Each f In b
                Dim k = f.K
                Dim vb = f.V
                If Not mp.ContainsKey(k) Then
                    mp(k) = vb
                Else
                    Dim va = mp(k)
                    If va = vb Then Continue For
                    Dim vaVar = st.IsVar(va)
                    Dim vbVar = st.IsVar(vb)
                    If vaVar AndAlso Not vbVar Then
                        mp(k) = vb
                    ElseIf (Not vaVar) AndAlso vbVar Then
                        ' keep va
                    ElseIf vaVar AndAlso vbVar Then
                        ' keep
                    Else
                        Return Nothing
                    End If
                End If
            Next
            Dim out As New List(Of Feat)(mp.Count)
            For Each kv In mp
                out.Add(New Feat(kv.Key, kv.Value))
            Next
            Return Norm(out)
        End Function

        Public Function Require(st As Symtab, fs As List(Of Feat), k As Integer, v As Integer) As List(Of Feat)
            Dim tmp As New List(Of Feat) From {New Feat(k, v)}
            Return Unify(st, fs, Norm(tmp))
        End Function
    End Module

    ' =========================
    ' Data structures
    ' =========================
    Public Structure Token
        Public Raw As String
        Public Text As String
        Public Index As Integer
        Public Sub New(raw As String, text As String, idx As Integer)
            Me.Raw = raw : Me.Text = text : Me.Index = idx
        End Sub
    End Structure

    Public Class LexEntry
        Public Pos As Integer
        Public Weight As Double
        Public Feats As List(Of Feat)
    End Class

    Public Structure PairKey
        Public A As Integer
        Public B As Integer
        Public Sub New(a As Integer, b As Integer)
            Me.A = a : Me.B = b
        End Sub
    End Structure

    Public Class PairKeyComparer
        Implements IEqualityComparer(Of PairKey)
        Public Overloads Function Equals(x As PairKey, y As PairKey) As Boolean Implements IEqualityComparer(Of PairKey).Equals
            Return x.A = y.A AndAlso x.B = y.B
        End Function
        Public Overloads Function GetHashCode(obj As PairKey) As Integer Implements IEqualityComparer(Of PairKey).GetHashCode
            Return (obj.A * 16777619) Xor obj.B
        End Function
    End Class

    Public Class Rule
        Public Lhs As Integer
        Public RhsLen As Integer
        Public Rhs1 As Integer
        Public Rhs2 As Integer ' only if len=2
        Public Weight As Double
        Public Op As String
        Public ArgKey As Integer?
        Public ArgVal As Integer?
        Public ArgType As Integer?
        Public PropIdxToRight As Boolean
    End Class

    Public Class Node
        Public Label As Integer
        Public IsLeaf As Boolean
        Public LeafRaw As String
        Public Feats As List(Of Feat)
        Public Score As Double
        Public Left As Integer?   ' node id
        Public Right As Integer?  ' node id
        Public Child As Integer?  ' node id (unary)
    End Class

    Public Class Item
        Public Cat As Integer
        Public Feats As List(Of Feat)
        Public FeatsH As ULong
        Public Score As Double
        Public NodeId As Integer
    End Class

    Public Class Arena
        Private ReadOnly nodes As New List(Of Node)

        Public Function Add(n As Node) As Integer
            Dim id = nodes.Count
            nodes.Add(n)
            Return id
        End Function

        Public Function GetNode(id As Integer) As Node
            Return nodes(id)
        End Function

        Public Function CloneReplace(rootId As Integer, fromV As Integer, toV As Integer) As Integer
            Dim n0 = GetNode(rootId)
            Dim feats2 = If(n0.Feats Is Nothing, New List(Of Feat)(), Feats.ReplaceValue(n0.Feats, fromV, toV))

            Dim child2 As Integer? = Nothing
            If n0.Child.HasValue Then child2 = CloneReplace(n0.Child.Value, fromV, toV)
            Dim left2 As Integer? = Nothing
            If n0.Left.HasValue Then left2 = CloneReplace(n0.Left.Value, fromV, toV)
            Dim right2 As Integer? = Nothing
            If n0.Right.HasValue Then right2 = CloneReplace(n0.Right.Value, fromV, toV)

            Dim n2 As New Node With {
                .Label = n0.Label, .IsLeaf = n0.IsLeaf, .LeafRaw = n0.LeafRaw,
                .Feats = feats2, .Score = n0.Score,
                .Left = left2, .Right = right2, .Child = child2
            }
            Return Add(n2)
        End Function

        Public Function Pretty(st As Symtab, nodeId As Integer) As String
            Dim sb As New StringBuilder()
            PrettyRec(st, nodeId, 0, sb)
            Return sb.ToString()
        End Function

        Private Sub PrettyRec(st As Symtab, nodeId As Integer, indent As Integer, sb As StringBuilder)
            Dim n = GetNode(nodeId)
            sb.Append(New String(" "c, indent * 2))
            If n.IsLeaf Then
                sb.AppendLine(n.LeafRaw)
                Return
            End If
            Dim label = st.Str(n.Label)
            sb.Append(label)
            If n.Feats IsNot Nothing AndAlso n.Feats.Count > 0 Then
                sb.Append(" [")
                For i = 0 To n.Feats.Count - 1
                    If i > 0 Then sb.Append(", ")
                    sb.Append(st.Str(n.Feats(i).K)).Append("=").Append(st.Str(n.Feats(i).V))
                Next
                sb.Append("]")
            End If
            sb.Append("  (score=").Append(n.Score.ToString("0.000")).AppendLine(")")

            If n.Child.HasValue Then
                PrettyRec(st, n.Child.Value, indent + 1, sb)
            Else
                If n.Left.HasValue Then PrettyRec(st, n.Left.Value, indent + 1, sb)
                If n.Right.HasValue Then PrettyRec(st, n.Right.Value, indent + 1, sb)
            End If
        End Sub
    End Class

    Public Class Bucket
        Public Items As New List(Of Item) ' desc score
        Public Hashes As New HashSet(Of ULong)

        Public Function HasHash(h As ULong) As Boolean
            Return Hashes.Contains(h)
        End Function

        Public Function Insert(it As Item, beam As Integer) As Integer
            Hashes.Add(it.FeatsH)
            Dim i = 0
            While i < Items.Count AndAlso Items(i).Score >= it.Score
                i += 1
            End While
            Items.Insert(i, it)
            If Items.Count > beam Then
                Dim pruned = Items.Count - beam
                Items.RemoveRange(beam, pruned)
                Return pruned
            End If
            Return 0
        End Function
    End Class

    ' =========================
    ' Constants
    ' =========================
    Public Class Consts
        Public idx, qi, gap, obl, yes, fin, no, gen, num, sg As Integer
        Public TOK, S, VP_FIN, Pinf, VP_NF, VP, Cl As Integer
        Public N, PropN, Pron, Adv, Vi, Vt As Integer
    End Class

    Private Function EnsureConstants(st As Symtab) As Consts
        Dim c As New Consts With {
            .idx = st.Intern("idx"),
            .qi = st.Intern("?i"),
            .gap = st.Intern("gap"),
            .obl = st.Intern("obl"),
            .yes = st.Intern("yes"),
            .fin = st.Intern("fin"),
            .no = st.Intern("no"),
            .gen = st.Intern("gen"),
            .num = st.Intern("num"),
            .sg = st.Intern("sg"),
            .TOK = st.Intern("TOK"),
            .S = st.Intern("S"),
            .VP_FIN = st.Intern("VP_FIN"),
            .Pinf = st.Intern("Pinf"),
            .VP_NF = st.Intern("VP_NF"),
            .VP = st.Intern("VP"),
            .Cl = st.Intern("Cl"),
            .N = st.Intern("N"),
            .PropN = st.Intern("PropN"),
            .Pron = st.Intern("Pron"),
            .Adv = st.Intern("Adv"),
            .Vi = st.Intern("Vi"),
            .Vt = st.Intern("Vt")
        }
        Return c
    End Function

    ' =========================
    ' Tokenization
    ' =========================
    Private Function SplitSentences(text As String) As List(Of String)
        Dim parts = text.Split(New Char() {"."c, vbLf(0), vbCr(0)}, StringSplitOptions.RemoveEmptyEntries)
        Dim out As New List(Of String)
        For Each p In parts
            Dim s = p.Trim()
            If s.Length > 0 Then out.Add(s)
        Next
        Return out
    End Function

    Private Function EncliticSplit(raw As String, low As String, idx As Integer) As List(Of Token)
        Dim clitics = New String() {"me", "te", "se", "lo", "la", "los", "las", "le", "les", "nos", "os"}
        Dim best As String = Nothing
        For Each c In clitics
            If low.EndsWith(c, StringComparison.Ordinal) Then
                If best Is Nothing OrElse c.Length > best.Length Then best = c
            End If
        Next
        If best Is Nothing Then Return Nothing

        Dim baseLen = low.Length - best.Length
        If baseLen <= 2 Then Return Nothing
        Dim base = low.Substring(0, baseLen)

        Dim looksVerb = base.EndsWith("ar") OrElse base.EndsWith("er") OrElse base.EndsWith("ir") OrElse
                        base.EndsWith("ando") OrElse base.EndsWith("iendo")
        If Not looksVerb Then Return Nothing

        Dim rawBase = raw.Substring(0, baseLen)
        Dim rawCl = raw.Substring(baseLen)
        Dim t1 As New Token(rawBase, rawBase.ToLowerInvariant(), idx)
        Dim t2 As New Token(rawCl, rawCl.ToLowerInvariant(), idx + 1)
        Return New List(Of Token) From {t1, t2}
    End Function

    Private Function Tokenize(sent As String) As Listser
        Dim re As New Regex("[\p{L}]+(?:[-'][\p{L}]+)*", RegexOptions.CultureInvariant)
        Dim out As New List(Of Token)
        Dim idx = 0
        For Each m As Match In re.Matches(sent)
            Dim raw = m.Value
            Dim low = raw.ToLowerInvariant()

            If low = "al" Then
                out.Add(New Token("a", "a", idx)) : idx += 1
                out.Add(New Token("el", "el", idx)) : idx += 1
                Continue For
            ElseIf low = "del" Then
                out.Add(New Token("de", "de", idx)) : idx += 1
                out.Add(New Token("el", "el", idx)) : idx += 1
                Continue For
            End If

            Dim split = EncliticSplit(raw, low, idx)
            If split IsNot Nothing Then
                out.AddRange(split)
                idx += 2
            Else
                out.Add(New Token(raw, low, idx))
                idx += 1
            End If
        Next
        Return out
    End Function

    ' =========================
    ' Load lexicon / grammar JSON
    ' =========================
    Private Function LoadLexicon(st As Symtab, path As String) As Dictionary(Of String, List(Of LexEntry))
        Dim text = File.ReadAllText(path, Encoding.UTF8)
        Using doc = JsonDocument.Parse(text)
            Dim root = doc.RootElement
            Dim entries = root.GetProperty("entries")
            Dim lex As New Dictionary(Of String, List(Of LexEntry))(StringComparer.Ordinal)
            For Each wordProp In entries.EnumerateObject()
                Dim word = wordProp.Name
                Dim arr = wordProp.Value
                Dim list As New List(Of LexEntry)
                For Each obj In arr.EnumerateArray()
                    Dim posS = obj.GetProperty("pos").GetString()
                    Dim w = obj.GetProperty("weight").GetDouble()
                    Dim posId = st.Intern(posS)
                    Dim fs As New List(Of Feat)
                    If obj.TryGetProperty("feats", Nothing) Then
                        Dim featsObj = obj.GetProperty("feats")
                        For Each f In featsObj.EnumerateObject()
                            fs.Add(New Feat(st.Intern(f.Name), st.Intern(f.Value.GetString())))
                        Next
                    End If
                    fs = Feats.Norm(fs)
                    list.Add(New LexEntry With {.Pos = posId, .Weight = w, .Feats = fs})
                Next
                lex(word) = list
            Next
            Return lex
        End Using
    End Function

    Private Class Grammar
        Public Unary As New Dictionary(Of Integer, List(Of Rule))()
        Public Binary As New Dictionary(Of PairKey, List(Of Rule))(New PairKeyComparer())
    End Class

    Private Function LoadGrammar(st As Symtab, path As String) As Grammar
        Dim text = File.ReadAllText(path, Encoding.UTF8)
        Using doc = JsonDocument.Parse(text)
            Dim root = doc.RootElement
            Dim rulesJ = root.GetProperty("rules")
            Dim g As New Grammar()
            For Each r In rulesJ.EnumerateArray()
                Dim lhs = st.Intern(r.GetProperty("lhs").GetString())
                Dim rhs = r.GetProperty("rhs")
                Dim len = rhs.GetArrayLength()
                If len <> 1 AndAlso len <> 2 Then Throw New Exception("grammar.json: rhs len debe ser 1 o 2")
                Dim rhs1 = st.Intern(rhs(0).GetString())
                Dim rhs2 = If(len = 2, st.Intern(rhs(1).GetString()), -1)
                Dim w = r.GetProperty("weight").GetDouble()
                Dim op = If(r.TryGetProperty("op", Nothing), r.GetProperty("op").GetString(), "EMPTY")

                Dim argKey As Integer? = Nothing
                Dim argVal As Integer? = Nothing
                Dim argType As Integer? = Nothing
                If r.TryGetProperty("args", Nothing) Then
                    Dim a = r.GetProperty("args")
                    If a.TryGetProperty("key", Nothing) Then argKey = st.Intern(a.GetProperty("key").GetString())
                    If a.TryGetProperty("value", Nothing) Then argVal = st.Intern(a.GetProperty("value").GetString())
                    If a.TryGetProperty("type", Nothing) Then argType = st.Intern(a.GetProperty("type").GetString())
                End If

                Dim propIdx = False
                If r.TryGetProperty("post", Nothing) Then
                    For Each p In r.GetProperty("post").EnumerateArray()
                        If p.GetString() = "PROPAGATE_IDX_TO_RIGHT" Then propIdx = True
                    Next
                End If

                Dim ru As New Rule With {
                    .Lhs = lhs, .RhsLen = len, .Rhs1 = rhs1, .Rhs2 = rhs2,
                    .Weight = w, .Op = op,
                    .ArgKey = argKey, .ArgVal = argVal, .ArgType = argType,
                    .PropIdxToRight = propIdx
                }

                If len = 1 Then
                    If Not g.Unary.ContainsKey(rhs1) Then g.Unary(rhs1) = New List(Of Rule)
                    g.Unary(rhs1).Add(ru)
                Else
                    Dim key As New PairKey(rhs1, rhs2)
                    If Not g.Binary.ContainsKey(key) Then g.Binary(key) = New List(Of Rule)
                    g.Binary(key).Add(ru)
                End If
            Next
            Return g
        End Using
    End Function

    ' =========================
    ' OOV guess
    ' =========================
    Private Function DetWord(low As String) As Boolean
        Return low = "el" OrElse low = "la" OrElse low = "los" OrElse low = "las"
    End Function

    Private Function GuessLex(st As Symtab, c As Consts, tok As Token) As List(Of LexEntry)
        Dim raw = tok.Raw
        Dim low = tok.Text
        Dim out As New List(Of LexEntry)

        If raw.Length > 0 AndAlso Char.IsUpper(raw(0)) AndAlso Not DetWord(low) Then
            Dim fs As New List(Of Feat) From {New Feat(c.num, c.sg)}
            out.Add(New LexEntry With {.Pos = c.PropN, .Weight = 0.03, .Feats = Feats.Norm(fs)})
        End If

        If low.EndsWith("mente", StringComparison.Ordinal) Then
            out.Add(New LexEntry With {.Pos = c.Adv, .Weight = -0.03, .Feats = New List(Of Feat)()})
        End If

        Dim baseV As New List(Of Feat) From {New Feat(c.fin, c.no), New Feat(c.obl, c.no)}
        baseV = Feats.Norm(baseV)

        If low.EndsWith("ar") OrElse low.EndsWith("er") OrElse low.EndsWith("ir") Then
            out.Add(New LexEntry With {.Pos = c.Vi, .Weight = -0.12, .Feats = baseV})
            out.Add(New LexEntry With {.Pos = c.Vt, .Weight = -0.14, .Feats = baseV})
        ElseIf low.EndsWith("ando") OrElse low.EndsWith("iendo") Then
            out.Add(New LexEntry With {.Pos = c.Vi, .Weight = -0.14, .Feats = baseV})
            out.Add(New LexEntry With {.Pos = c.Vt, .Weight = -0.16, .Feats = baseV})
        End If

        If out.Count = 0 Then
            Dim vg = st.Intern("?g")
            Dim vn = st.Intern("?n")
            Dim fs As New List(Of Feat) From {New Feat(c.gen, vg), New Feat(c.num, vn)}
            out.Add(New LexEntry With {.Pos = c.N, .Weight = -0.35, .Feats = Feats.Norm(fs)})
        End If

        Return out
    End Function

    ' =========================
    ' Ops DSL
    ' =========================
    Private Function ApplyOp(st As Symtab, c As Consts, ru As Rule, lf As List(Of Feat), rf As List(Of Feat)) As List(Of Feat)
        Select Case ru.Op
            Case "EMPTY"
                Return New List(Of Feat)()
            Case "LEFT"
                Return lf
            Case "RIGHT"
                Return rf
            Case "UNIFY"
                Return Feats.Unify(st, lf, rf)
            Case "REQUIRE_LEFT"
                If ru.ArgKey.HasValue AndAlso ru.ArgVal.HasValue Then
                    Return Feats.Require(st, lf, ru.ArgKey.Value, ru.ArgVal.Value)
                End If
                Return Nothing
            Case "REQUIRE_RIGHT"
                If ru.ArgKey.HasValue AndAlso ru.ArgVal.HasValue Then
                    Return Feats.Require(st, rf, ru.ArgKey.Value, ru.ArgVal.Value)
                End If
                Return Nothing
            Case "MAKE_GAP"
                If ru.ArgType.HasValue Then
                    Dim fs As New List(Of Feat) From {New Feat(c.idx, c.qi), New Feat(c.gap, ru.ArgType.Value)}
                    Return Feats.Norm(fs)
                End If
                Return Nothing
            Case "RELCLAUSE_OBL"
                Dim ok = Feats.Require(st, rf, c.obl, c.yes)
                If ok Is Nothing Then Return Nothing
                Return Feats.Unify(st, lf, Feats.Norm(New List(Of Feat) From {New Feat(c.gap, c.obl)}))
            Case Else
                Return Nothing
        End Select
    End Function

    ' =========================
    ' Chart helpers
    ' =========================
    Private Function Cidx(i As Integer, j As Integer, n As Integer) As Integer
        Return i * (n + 1) + j
    End Function

    Private Function ChartNew(n As Integer) As Dictionary(Of Integer, Bucket)()
        Dim size = (n + 1) * (n + 1)
        Dim arr(size - 1) As Dictionary(Of Integer, Bucket)
        For k = 0 To size - 1
            arr(k) = New Dictionary(Of Integer, Bucket)()
        Next
        Return arr
    End Function

    Private Function ChartGet(chart As Dictionary(Of Integer, Bucket)(), i As Integer, j As Integer, n As Integer) As Dictionary(Of Integer, Bucket)
        Return chart(Cidx(i, j, n))
    End Function

    ' =========================
    ' Unary closure
    ' =========================
    Private Function UnaryClosure(st As Symtab, c As Consts, g As Grammar,
                                 cell As Dictionary(Of Integer, Bucket),
                                 arena As Arena, beam As Integer,
                                 ByRef pruned As Integer, ByRef unaryApps As Integer) As Dictionary(Of Integer, Bucket)

        Dim changed As Boolean
        Do
            changed = False
            Dim cats As New List(Of Integer)(cell.Keys)
            For Each rhsCat In cats
                Dim bk = cell(rhsCat)
                If Not g.Unary.ContainsKey(rhsCat) Then Continue For
                Dim rules = g.Unary(rhsCat)
                Dim itemsSnap = New List(Of Item)(bk.Items)
                For Each ru In rules
                    For Each child In itemsSnap
                        Dim pf = ApplyOp(st, c, ru, child.Feats, New List(Of Feat)())
                        If pf Is Nothing Then Continue For
                        unaryApps += 1
                        Dim score = child.Score + ru.Weight
                        Dim node As New Node With {.Label = ru.Lhs, .IsLeaf = False, .LeafRaw = Nothing,
                                                  .Feats = pf, .Score = score,
                                                  .Left = Nothing, .Right = Nothing, .Child = child.NodeId}
                        Dim nid = arena.Add(node)
                        Dim it As New Item With {.Cat = ru.Lhs, .Feats = pf, .FeatsH = Feats.Hash64(pf),
                                                 .Score = score, .NodeId = nid}
                        Dim bk2 As Bucket = Nothing
                        If Not cell.TryGetValue(it.Cat, bk2) Then
                            bk2 = New Bucket()
                            cell(it.Cat) = bk2
                        End If
                        If Not bk2.HasHash(it.FeatsH) Then
                            pruned += bk2.Insert(it, beam)
                            changed = True
                        End If
                    Next
                Next
            Next
        Loop While changed

        Return cell
    End Function

    ' =========================
    ' Emit lexical
    ' =========================
    Private Sub EmitEntries(st As Symtab, c As Consts,
                            entries As List(Of LexEntry), tok As Token,
                            arena As Arena, cell As Dictionary(Of Integer, Bucket),
                            beam As Integer, tids As Dictionary(Of Integer, Integer),
                            ByRef pruned As Integer)

        For Each e In entries
            Dim pos = e.Pos
            Dim w = e.Weight
            Dim fs0 = e.Feats
            Dim fs1 = fs0

            If pos = c.N OrElse pos = c.PropN OrElse pos = c.Pron Then
                If Not Feats.Find(fs0, c.idx).HasValue Then
                    Dim tid = tids(tok.Index)
                    Dim uni = Feats.Unify(st, fs0, Feats.Norm(New List(Of Feat) From {New Feat(c.idx, tid)}))
                    If uni IsNot Nothing Then fs1 = uni
                End If
            End If

            Dim leaf As New Node With {.Label = c.TOK, .IsLeaf = True, .LeafRaw = tok.Raw,
                                       .Feats = New List(Of Feat)(), .Score = w,
                                       .Left = Nothing, .Right = Nothing, .Child = Nothing}
            Dim leafId = arena.Add(leaf)
            Dim pre As New Node With {.Label = pos, .IsLeaf = False, .LeafRaw = Nothing,
                                      .Feats = fs1, .Score = w,
                                      .Left = Nothing, .Right = Nothing, .Child = leafId}
            Dim preId = arena.Add(pre)

            Dim it As New Item With {.Cat = pos, .Feats = fs1, .FeatsH = Feats.Hash64(fs1), .Score = w, .NodeId = preId}
            Dim bk As Bucket = Nothing
            If Not cell.TryGetValue(pos, bk) Then
                bk = New Bucket()
                cell(pos) = bk
            End If
            If Not bk.HasHash(it.FeatsH) Then
                pruned += bk.Insert(it, beam)
            End If
        Next
    End Sub

    ' =========================
    ' Sanity checks
    ' =========================
    Private Function HasDescLabel(arena As Arena, nodeId As Integer, label As Integer) As Boolean
        Dim n = arena.GetNode(nodeId)
        If (Not n.IsLeaf) AndAlso n.Label = label Then Return True
        If n.Child.HasValue AndAlso HasDescLabel(arena, n.Child.Value, label) Then Return True
        If n.Left.HasValue AndAlso HasDescLabel(arena, n.Left.Value, label) Then Return True
        If n.Right.HasValue AndAlso HasDescLabel(arena, n.Right.Value, label) Then Return True
        Return False
    End Function

    Private Function SanitySHasVpFin(arena As Arena, c As Consts, rootId As Integer) As Boolean
        Dim s = c.S, vpfin = c.VP_FIN
        Dim stack As New Stack(Of Integer)
        stack.Push(rootId)
        While stack.Count > 0
            Dim id = stack.Pop()
            Dim n = arena.GetNode(id)
            If Not n.IsLeaf AndAlso n.Label = s Then
                If n.Child.HasValue AndAlso arena.GetNode(n.Child.Value).Label = vpfin Then Return True
                If n.Left.HasValue AndAlso arena.GetNode(n.Left.Value).Label = vpfin Then Return True
                If n.Right.HasValue AndAlso arena.GetNode(n.Right.Value).Label = vpfin Then Return True
            End If
            If n.Child.HasValue Then stack.Push(n.Child.Value)
            If n.Left.HasValue Then stack.Push(n.Left.Value)
            If n.Right.HasValue Then stack.Push(n.Right.Value)
        End While
        Return False
    End Function

    Private Function SanitySinTakesVpNf(arena As Arena, c As Consts, rootId As Integer) As Boolean
        Dim pinf = c.Pinf, vpnf = c.VP_NF
        Dim stack As New Stack(Of Integer)
        stack.Push(rootId)
        While stack.Count > 0
            Dim id = stack.Pop()
            Dim n = arena.GetNode(id)
            If Not n.IsLeaf AndAlso n.Label = pinf Then
                If Not HasDescLabel(arena, id, vpnf) Then Return False
            End If
            If n.Child.HasValue Then stack.Push(n.Child.Value)
            If n.Left.HasValue Then stack.Push(n.Left.Value)
            If n.Right.HasValue Then stack.Push(n.Right.Value)
        End While
        Return True
    End Function

    Private Function SanityEncliticOnlyNf(arena As Arena, c As Consts, rootId As Integer) As Boolean
        Dim vp = c.VP, cl = c.Cl, vt = c.Vt, vi = c.Vi
        Dim stack As New Stack(Of Integer)
        stack.Push(rootId)
        While stack.Count > 0
            Dim id = stack.Pop()
            Dim n = arena.GetNode(id)
            If Not n.IsLeaf AndAlso n.Label = vp AndAlso n.Left.HasValue AndAlso n.Right.HasValue Then
                Dim ln = arena.GetNode(n.Left.Value)
                Dim rn = arena.GetNode(n.Right.Value)
                If (Not rn.IsLeaf) AndAlso rn.Label = cl AndAlso (Not ln.IsLeaf) AndAlso (ln.Label = vt OrElse ln.Label = vi) Then
                    Return False
                End If
            End If
            If n.Child.HasValue Then stack.Push(n.Child.Value)
            If n.Left.HasValue Then stack.Push(n.Left.Value)
            If n.Right.HasValue Then stack.Push(n.Right.Value)
        End While
        Return True
    End Function

    ' =========================
    ' Parse sentence (CKY)
    ' =========================
    Private Function ParseSentence(st As Symtab, c As Consts, lex As Dictionary(Of String, List(Of LexEntry)),
                                   g As Grammar, sent As String,
                                   beam As Integer, topk As Integer,
                                   wantTrees As Boolean, wantPrint As Boolean) As Dictionary(Of String, Object)

        Dim t0 = Environment.TickCount64
        Dim toks = Tokenize(sent)
        Dim n = toks.Count

        If n = 0 Then
            Return New Dictionary(Of String, Object) From {
                {"sentence", sent}, {"tokens", 0}, {"oovTokens", 0},
                {"parsed", False}, {"nParsesReturned", 0},
                {"bestScore", Nothing}, {"timeMs", 0.0},
                {"chartItemsTotal", 0}, {"chartItemsMaxCell", 0},
                {"prunedByBeam", 0}, {"unaryApplications", 0},
                {"ambiguousCells", 0},
                {"sanitySHasVpFin", False}, {"sanitySinTakesVpNf", False}, {"sanityEncliticOnlyNf", False},
                {"notes", New List(Of String) From {"empty"}}, {"bestTree", Nothing}
            }
        End If

        Dim tids As New Dictionary(Of Integer, Integer)
        For i = 0 To n - 1
            tids(i) = st.Intern($"t{i}")
        Next

        Dim chart = ChartNew(n)
        Dim arena As New Arena()

        Dim oov = 0
        Dim prunedTotal = 0
        Dim unaryTotal = 0

        ' lexical init
        For i = 0 To n - 1
            Dim tok = toks(i)
            Dim cell = ChartGet(chart, i, i + 1, n)

            Dim entries As List(Of LexEntry) = Nothing
            If Not lex.TryGetValue(tok.Text, entries) Then
                oov += 1
                entries = GuessLex(st, c, tok)
            End If

            EmitEntries(st, c, entries, tok, arena, cell, beam, tids, prunedTotal)
            UnaryClosure(st, c, g, cell, arena, beam, prunedTotal, unaryTotal)
        Next

        ' CKY
        For span = 2 To n
            For i = 0 To n - span
                Dim j = i + span
                Dim cell = ChartGet(chart, i, j, n)
                For k = i + 1 To j - 1
                    Dim lcell = ChartGet(chart, i, k, n)
                    Dim rcell = ChartGet(chart, k, j, n)
                    If lcell.Count = 0 OrElse rcell.Count = 0 Then Continue For

                    For Each kvL In lcell
                        Dim catL = kvL.Key
                        Dim itemsL = kvL.Value.Items
                        For Each kvR In rcell
                            Dim catR = kvR.Key
                            Dim itemsR = kvR.Value.Items
                            Dim rules As List(Of Rule) = Nothing
                            Dim key As New PairKey(catL, catR)
                            If Not g.Binary.TryGetValue(key, rules) Then Continue For

                            For Each ru In rules
                                For Each il In itemsL
                                    For Each ir In itemsR
                                        Dim pf = ApplyOp(st, c, ru, il.Feats, ir.Feats)
                                        If pf Is Nothing Then Continue For
                                        Dim score = il.Score + ir.Score + ru.Weight

                                        Dim rightNode = ir.NodeId
                                        If ru.PropIdxToRight Then
                                            Dim idxv = Feats.Find(il.Feats, c.idx)
                                            If idxv.HasValue Then
                                                rightNode = arena.CloneReplace(ir.NodeId, c.qi, idxv.Value)
                                            End If
                                        End If

                                        Dim node As New Node With {.Label = ru.Lhs, .IsLeaf = False, .LeafRaw = Nothing,
                                                                  .Feats = pf, .Score = score,
                                                                  .Left = il.NodeId, .Right = rightNode, .Child = Nothing}
                                        Dim nid = arena.Add(node)
                                        Dim it As New Item With {.Cat = ru.Lhs, .Feats = pf, .FeatsH = Feats.Hash64(pf),
                                                                 .Score = score, .NodeId = nid}
                                        Dim bk As Bucket = Nothing
                                        If Not cell.TryGetValue(it.Cat, bk) Then
                                            bk = New Bucket()
                                            cell(it.Cat) = bk
                                        End If
                                        If Not bk.HasHash(it.FeatsH) Then
                                            prunedTotal += bk.Insert(it, beam)
                                        End If
                                    Next
                                Next
                            Next
                        Next
                    Next
                Next
                UnaryClosure(st, c, g, cell, arena, beam, prunedTotal, unaryTotal)
            Next
        Next

        ' metrics
        Dim totItems = 0
        Dim maxCell = 0
        Dim ambCells = 0
        For Each cell In chart
            Dim count = 0
            If cell.Count >= 2 Then ambCells += 1
            For Each bk In cell.Values
                count += bk.Items.Count
            Next
            totItems += count
            If count > maxCell Then maxCell = count
        Next

        ' best S
        Dim cellSN = ChartGet(chart, 0, n, n)
        Dim parsed = False
        Dim bestScoreObj As Object = Nothing
        Dim nRet = 0
        Dim notes As New List(Of String)
        Dim bestTreeObj As Object = Nothing
        Dim s1 = False, s2 = False, s3 = False

        If cellSN.ContainsKey(c.S) AndAlso cellSN(c.S).Items.Count > 0 Then
            parsed = True
            Dim itemsS = cellSN(c.S).Items
            nRet = Math.Min(topk, itemsS.Count)
            Dim best = itemsS(0)
            bestScoreObj = best.Score
            s1 = SanitySHasVpFin(arena, c, best.NodeId)
            s2 = SanitySinTakesVpNf(arena, c, best.NodeId)
            s3 = SanityEncliticOnlyNf(arena, c, best.NodeId)
            If Not s1 Then notes.Add("WARN: S sin VP_FIN visible")
            If Not s2 Then notes.Add("WARN: 'sin' sin VP_NF bajo Pinf")
            If Not s3 Then notes.Add("WARN: enclítico con verbo finito")
            If wantTrees Then bestTreeObj = arena.Pretty(st, best.NodeId)
        Else
            notes.Add("NO_PARSE")
        End If

        Dim timeMs = CDbl(Environment.TickCount64 - t0)

        If wantPrint Then
            Console.WriteLine("==============================================================================")
            Console.WriteLine(sent)
            Console.WriteLine($"tokens={n}  oov={oov}  parsed={(If(parsed, 1, 0))}  parses={nRet}  bestScore={(If(bestScoreObj Is Nothing, "null", CType(bestScoreObj, Double).ToString("0.000000")))}  time_ms={timeMs:0.0}")
            Console.WriteLine($"chart_items={totItems}  max_cell={maxCell}  pruned={prunedTotal}  unary_apps={unaryTotal}  amb_cells={ambCells}")
            If notes.Count > 0 Then Console.WriteLine("notes: " & String.Join("; ", notes))
            If wantTrees AndAlso bestTreeObj IsNot Nothing Then Console.WriteLine(CType(bestTreeObj, String))
        End If

        Return New Dictionary(Of String, Object) From {
            {"sentence", sent},
            {"tokens", n},
            {"oovTokens", oov},
            {"parsed", parsed},
            {"nParsesReturned", nRet},
            {"bestScore", bestScoreObj},
            {"timeMs", timeMs},
            {"chartItemsTotal", totItems},
            {"chartItemsMaxCell", maxCell},
            {"prunedByBeam", prunedTotal},
            {"unaryApplications", unaryTotal},
            {"ambiguousCells", ambCells},
            {"sanitySHasVpFin", s1},
            {"sanitySinTakesVpNf", s2},
            {"sanityEncliticOnlyNf", s3},
            {"notes", notes},
            {"bestTree", bestTreeObj}
        }
    End Function

    ' =========================
    ' CLI + main
    ' =========================
    Private Function GetArg(args As String(), name As String) As String
        For i = 0 To args.Length - 2
            If args(i) = name Then Return args(i + 1)
        Next
        Return Nothing
    End Function

    Private Function HasFlag(args As String(), name As String) As Boolean
        For Each a In args
            If a = name Then Return True
        Next
        Return False
    End Function

    Sub Main()
        Dim args = Environment.GetCommandLineArgs()
        Dim passthru As New List(Of String)
        Dim seenSep = False
        For i = 1 To args.Length - 1
            If args(i) = "--" Then
                seenSep = True
                Continue For
            End If
            If seenSep Then passthru.Add(args(i))
        Next
        If Not seenSep Then
            ' allow direct usage without --
            passthru.AddRange(args.Skip(1))
        End If

        Dim a = passthru.ToArray()

        If HasFlag(a, "--help") Then
            Console.WriteLine("Uso: dotnet run -- [--lex lexicon.json] [--grammar grammar.json] [--file corpus.txt | --text ""...""] [--beam 16] [--topk 1] [--trees] [--print] [--json out.json]")
            Return
        End If

        Dim lexPath = If(GetArg(a, "--lex"), "lexicon.json")
        Dim gramPath = If(GetArg(a, "--grammar"), "grammar.json")
        Dim filePath = If(GetArg(a, "--file"), "corpus.txt")
        Dim textArg = GetArg(a, "--text")
        Dim jsonOut = GetArg(a, "--json")
        Dim beam = If(GetArg(a, "--beam"), "16")
        Dim topk = If(GetArg(a, "--topk"), "1")
        Dim wantTrees = HasFlag(a, "--trees")
        Dim wantPrint = HasFlag(a, "--print")

        Dim beamN = Integer.Parse(beam)
        Dim topkN = Integer.Parse(topk)

        Dim st As New Symtab()
        Dim lex = LoadLexicon(st, lexPath)
        Dim grammar = LoadGrammar(st, gramPath)
        Dim c = EnsureConstants(st)

        Dim corpus As String = If(textArg, File.ReadAllText(filePath, Encoding.UTF8))
        Dim sents = SplitSentences(corpus)

        Dim rows As New List(Of Dictionary(Of String, Object))
        Dim parsedCount = 0
        Dim totTok = 0
        Dim totOov = 0
        Dim totTime = 0.0

        For Each s In sents
            Dim row = ParseSentence(st, c, lex, grammar, s, beamN, topkN, wantTrees, wantPrint)
            rows.Add(row)
            If CBool(row("parsed")) Then parsedCount += 1
            totTok += CInt(row("tokens"))
            totOov += CInt(row("oovTokens"))
            totTime += CDbl(row("timeMs"))
        Next

        Dim sn = sents.Count
        Dim coverage = If(sn = 0, 0.0, CDbl(parsedCount) / sn)
        Dim avgTok = If(sn = 0, 0.0, CDbl(totTok) / sn)
        Dim avgOov = If(sn = 0, 0.0, CDbl(totOov) / sn)
        Dim avgTime = If(sn = 0, 0.0, totTime / sn)

        If wantPrint Then
            Console.WriteLine("==============================================================================")
            Console.WriteLine("SUMMARY")
            Console.WriteLine($"sentences={sn}  coverage={coverage:0.000}  avg_tokens={avgTok:0.00}  avg_oov={avgOov:0.00}  avg_time_ms={avgTime:0.0}  beam={beamN}  top_k={topkN}")
        Else
            Console.WriteLine($"SUMMARY: sentences={sn} coverage={coverage:0.000} avg_time_ms={avgTime:0.0} beam={beamN} top_k={topkN}")
        End If

        If jsonOut IsNot Nothing Then
            Dim summary As New Dictionary(Of String, Object) From {
                {"sentences", sn},
                {"coverage", coverage},
                {"avgTokens", avgTok},
                {"avgOov", avgOov},
                {"totalTimeMs", totTime},
                {"avgTimeMs", avgTime},
                {"beam", beamN},
                {"topK", topkN},
                {"rows", rows}
            }
            Dim opts As New JsonSerializerOptions With {.WriteIndented = True}
            File.WriteAllText(jsonOut, JsonSerializer.Serialize(summary, opts), Encoding.UTF8)
            Console.WriteLine("Wrote JSON: " & jsonOut)
        End If
    End Sub

End Module
