module nlp_parser_v7
  use iso_fortran_env, only: int64, real64
  use json_module,     only: json_file, json_core, json_value, json_rk, json_ik
  implicit none
  private

  public :: parser_t

  integer, parameter :: I4 = selected_int_kind(9)

  type :: feat_t
    integer(I4) :: k = 0
    integer(I4) :: v = 0
  end type

  type :: featlist_t
    type(feat_t), allocatable :: a(:)
    integer(I4) :: n = 0
  contains
    procedure :: clear => featlist_clear
    procedure :: sort  => featlist_sort
    procedure :: get   => featlist_get
    procedure :: put   => featlist_put
    procedure :: hash64 => featlist_hash64
  end type

  type :: lex_entry_t
    character(len=:), allocatable :: word
    integer(I4) :: pos = 0
    real(real64) :: w = 1.0_real64
    type(featlist_t) :: feats
  end type

  type :: constraint_t
    character(len=:), allocatable :: ctype   ! require / unify / agree / assign
    character(len=:), allocatable :: target  ! left/right/child/parent
    character(len=:), allocatable :: target2 ! (para unify)
    integer(I4) :: key = 0
    integer(I4) :: val = 0
    integer(I4) :: key2 = 0
  end type

  type :: rule_t
    integer(I4) :: lhs = 0
    integer(I4) :: rhs_len = 0
    integer(I4) :: rhs1 = 0
    integer(I4) :: rhs2 = 0
    real(real64) :: w = 1.0_real64
    character(len=:), allocatable :: propagate  ! merge/left/right
    type(constraint_t), allocatable :: cs(:)
    integer(I4) :: ncs = 0
  end type

  type :: node_t
    integer(I4) :: label = 0
    integer(I4) :: left = 0
    integer(I4) :: right = 0
    logical :: is_leaf = .false.
    character(len=:), allocatable :: leaf
  end type

  type :: item_t
    integer(I4) :: cat = 0
    type(featlist_t) :: feats
    integer(int64) :: fh = 0_int64
    real(real64) :: score = -1.0e300_real64
    integer(I4) :: node_id = 0
  end type

  type :: bucket_t
    type(item_t), allocatable :: items(:)
    integer(I4) :: n = 0
  contains
    procedure :: insert => bucket_insert
  end type

  type :: cell_t
    integer(I4), allocatable :: cats(:)
    type(bucket_t), allocatable :: bucks(:)
    integer(I4) :: n = 0
  contains
    procedure :: get_bucket => cell_get_bucket
    procedure :: add_item   => cell_add_item
  end type

  type :: symtab_t
    character(len=:), allocatable :: s(:)
    integer(I4) :: n = 0
  contains
    procedure :: intern => sym_intern
    procedure :: str    => sym_str
  end type

  type :: parser_t
    type(symtab_t) :: sym
    type(lex_entry_t), allocatable :: lex(:)
    integer(I4) :: nlex = 0
    type(rule_t), allocatable :: rules(:)
    integer(I4) :: nrules = 0
    integer(I4) :: start_sym = 0
    integer(I4) :: beam = 8

    type(node_t), allocatable :: arena(:)
    integer(I4) :: narena = 0
  contains
    procedure :: load_resources => parser_load_resources
    procedure :: parse_sentence => parser_parse_sentence
  end type

contains

  ! -------------------------
  ! String utils (ASCII lower; UTF-8 bytes preserved)
  ! -------------------------
  pure function lower_ascii(s) result(out)
    character(len=*), intent(in) :: s
    character(len=len(s)) :: out
    integer :: i, c
    out = s
    do i = 1, len(s)
      c = iachar(out(i:i))
      if (c >= iachar('A') .and. c <= iachar('Z')) then
        out(i:i) = achar(c + 32)
      end if
    end do
  end function

  pure logical function is_word_byte(ch)
    character(len=1), intent(in) :: ch
    integer :: c
    c = iachar(ch)
    is_word_byte = ( (c >= iachar('A') .and. c <= iachar('Z')) .or. &
                     (c >= iachar('a') .and. c <= iachar('z')) .or. &
                     (c >= iachar('0') .and. c <= iachar('9')) .or. &
                     (c >= 128) .or. ch == '_' )
  end function

  subroutine tokenize(sentence, toks, ntok)
    character(len=*), intent(in) :: sentence
    character(len=:), allocatable, intent(out) :: toks(:)
    integer(I4), intent(out) :: ntok

    character(len=:), allocatable :: buf
    character(len=1) :: ch
    integer :: i, n, cap
    character(len=:), allocatable :: tmp(:)

    n = len_trim(sentence)
    cap = max(8, n/2)
    allocate(tmp(cap))
    ntok = 0
    buf = ""

    do i = 1, n
      ch = sentence(i:i)
      if (is_word_byte(ch)) then
        buf = buf // ch
      else
        if (len(buf) > 0) then
          call push_token(tmp, cap, ntok, buf)
          buf = ""
        end if
      end if
    end do
    if (len(buf) > 0) call push_token(tmp, cap, ntok, buf)

    allocate(toks(ntok))
    toks = tmp(1:ntok)
  end subroutine

  subroutine push_token(a, cap, n, t)
    character(len=:), allocatable, intent(inout) :: a(:)
    integer, intent(inout) :: cap
    integer(I4), intent(inout) :: n
    character(len=*), intent(in) :: t
    character(len=:), allocatable :: b(:)
    if (n+1 > cap) then
      cap = cap * 2
      allocate(b(cap))
      b(1:n) = a(1:n)
      call move_alloc(b, a)
    end if
    n = n + 1
    a(n) = t
  end subroutine

  subroutine split_contractions_and_enclitics(toks_in, n_in, toks_out, n_out)
    character(len=:), allocatable, intent(in) :: toks_in(:)
    integer(I4), intent(in) :: n_in
    character(len=:), allocatable, intent(out) :: toks_out(:)
    integer(I4), intent(out) :: n_out

    character(len=:), allocatable :: tmp(:)
    integer :: cap, i
    character(len=:), allocatable :: t, tl, base, suf

    cap = max(8, n_in*2)
    allocate(tmp(cap))
    n_out = 0

    do i = 1, n_in
      t = toks_in(i)
      tl = lower_ascii(t)

      if (trim(tl) == "al") then
        call push_token(tmp, cap, n_out, "a")
        call push_token(tmp, cap, n_out, "el")
      else if (trim(tl) == "del") then
        call push_token(tmp, cap, n_out, "de")
        call push_token(tmp, cap, n_out, "el")
      else
        ! enclítico simple (heurística)
        call maybe_split_enclitic(t, base, suf)
        if (len_trim(suf) > 0) then
          call push_token(tmp, cap, n_out, base)
          call push_token(tmp, cap, n_out, suf)
        else
          call push_token(tmp, cap, n_out, t)
        end if
      end if
    end do

    allocate(toks_out(n_out))
    toks_out = tmp(1:n_out)
  end subroutine

  subroutine maybe_split_enclitic(tok, base, suf)
    character(len=*), intent(in) :: tok
    character(len=:), allocatable, intent(out) :: base, suf
    character(len=:), allocatable :: tl
    character(len=*), parameter :: sufs(12) = [ &
      "me","te","se","lo","la","los","las","le","les","nos","os","l" ]
    integer :: i, lt, ls

    tl = lower_ascii(tok)
    lt = len_trim(tl)
    base = tok
    suf  = ""

    do i = 1, size(sufs)
      ls = len_trim(sufs(i))
      if (lt > ls+2) then
        if (trim(tl(lt-ls+1:lt)) == trim(sufs(i))) then
          ! heurística: verbos suelen terminar en r/d/n (ASCII)
          if (tok(lt-ls:lt-ls) == "r" .or. tok(lt-ls:lt-ls) == "d" .or. tok(lt-ls:lt-ls) == "n") then
            base = tok(1:lt-ls)
            suf  = tok(lt-ls+1:lt)
            return
          end if
        end if
      end if
    end do
  end subroutine

  ! -------------------------
  ! Symtab
  ! -------------------------
  integer(I4) function sym_intern(this, s) result(id)
    class(symtab_t), intent(inout) :: this
    character(len=*), intent(in) :: s
    integer :: i
    character(len=:), allocatable :: ss
    ss = trim(s)
    do i = 1, this%n
      if (this%s(i) == ss) then
        id = i; return
      end if
    end do
    this%n = this%n + 1
    if (.not. allocated(this%s)) then
      allocate(this%s(8))
    else if (this%n > size(this%s)) then
      call grow_symtab(this)
    end if
    this%s(this%n) = ss
    id = this%n
  end function

  subroutine grow_symtab(this)
    class(symtab_t), intent(inout) :: this
    character(len=:), allocatable :: b(:)
    integer :: newcap, i
    newcap = max(8, 2*size(this%s))
    allocate(b(newcap))
    do i=1,this%n-1
      b(i) = this%s(i)
    end do
    call move_alloc(b, this%s)
  end subroutine

  function sym_str(this, id) result(s)
    class(symtab_t), intent(in) :: this
    integer(I4), intent(in) :: id
    character(len=:), allocatable :: s
    if (id <= 0 .or. id > this%n) then
      s = "<?>"
    else
      s = this%s(id)
    end if
  end function

  ! -------------------------
  ! Featlist
  ! -------------------------
  subroutine featlist_clear(this)
    class(featlist_t), intent(inout) :: this
    this%n = 0
    if (allocated(this%a)) deallocate(this%a)
  end subroutine

  subroutine featlist_sort(this)
    class(featlist_t), intent(inout) :: this
    integer :: i, j
    type(feat_t) :: x
    if (.not. allocated(this%a)) return
    do i = 2, this%n
      x = this%a(i)
      j = i - 1
      do while (j >= 1)
        if (this%a(j)%k < x%k) exit
        if (this%a(j)%k == x%k .and. this%a(j)%v <= x%v) exit
        this%a(j+1) = this%a(j)
        j = j - 1
      end do
      this%a(j+1) = x
    end do
  end subroutine

  integer(I4) function featlist_get(this, key) result(val)
    class(featlist_t), intent(in) :: this
    integer(I4), intent(in) :: key
    integer :: i
    val = 0
    do i=1,this%n
      if (this%a(i)%k == key) then
        val = this%a(i)%v; return
      end if
    end do
  end function

  subroutine featlist_put(this, key, val)
    class(featlist_t), intent(inout) :: this
    integer(I4), intent(in) :: key, val
    integer :: i
    if (.not. allocated(this%a)) then
      allocate(this%a(8))
      this%n = 0
    end if
    do i=1,this%n
      if (this%a(i)%k == key) then
        this%a(i)%v = val
        return
      end if
    end do
    this%n = this%n + 1
    if (this%n > size(this%a)) call grow_feats(this)
    this%a(this%n)%k = key
    this%a(this%n)%v = val
  end subroutine

  subroutine grow_feats(this)
    class(featlist_t), intent(inout) :: this
    type(feat_t), allocatable :: b(:)
    integer :: newcap
    newcap = max(8, 2*size(this%a))
    allocate(b(newcap))
    b(1:this%n-1) = this%a(1:this%n-1)
    call move_alloc(b, this%a)
  end subroutine

  integer(int64) function featlist_hash64(this) result(h)
    class(featlist_t), intent(in) :: this
    integer :: i
    integer(int64), parameter :: FNV_OFFSET = int(z'CBF29CE484222325', int64)
    integer(int64), parameter :: FNV_PRIME  = int(z'00000100000001B3', int64)
    h = FNV_OFFSET
    do i=1,this%n
      h = ieor(h, int(this%a(i)%k, int64)); h = h * FNV_PRIME
      h = ieor(h, int(this%a(i)%v, int64)); h = h * FNV_PRIME
    end do
  end function

  ! -------------------------
  ! Cell / bucket
  ! -------------------------
  integer(I4) function cell_get_bucket(this, cat) result(idx)
    class(cell_t), intent(inout) :: this
    integer(I4), intent(in) :: cat
    integer :: i
    idx = 0
    do i=1,this%n
      if (this%cats(i) == cat) then
        idx = i; return
      end if
    end do
    this%n = this%n + 1
    if (.not. allocated(this%cats)) then
      allocate(this%cats(8))
      allocate(this%bucks(8))
    else if (this%n > size(this%cats)) then
      call grow_cell(this)
    end if
    this%cats(this%n) = cat
    this%bucks(this%n)%n = 0
    if (allocated(this%bucks(this%n)%items)) deallocate(this%bucks(this%n)%items)
    idx = this%n
  end function

  subroutine grow_cell(this)
    class(cell_t), intent(inout) :: this
    integer(I4), allocatable :: bc(:)
    type(bucket_t), allocatable :: bb(:)
    integer :: newcap, i
    newcap = max(8, 2*size(this%cats))
    allocate(bc(newcap)); allocate(bb(newcap))
    bc(1:this%n-1) = this%cats(1:this%n-1)
    do i=1,this%n-1
      bb(i) = this%bucks(i)
    end do
    call move_alloc(bc, this%cats)
    call move_alloc(bb, this%bucks)
  end subroutine

  logical function cell_add_item(this, it, beam) result(inserted)
    class(cell_t), intent(inout) :: this
    type(item_t), intent(in) :: it
    integer(I4), intent(in) :: beam
    integer(I4) :: bidx
    bidx = this%get_bucket(it%cat)
    inserted = this%bucks(bidx)%insert(it, beam)
  end function

  logical function bucket_insert(this, it, beam) result(inserted)
    class(bucket_t), intent(inout) :: this
    type(item_t), intent(in) :: it
    integer(I4), intent(in) :: beam
    integer :: i, pos
    type(item_t), allocatable :: b(:)

    inserted = .false.

    ! dedup por hash de feats (beam chico => lineal ok)
    do i=1,this%n
      if (this%items(i)%fh == it%fh) then
        if (it%score > this%items(i)%score) then
          this%items(i) = it
          inserted = .true.
        end if
        return
      end if
    end do

    if (.not. allocated(this%items)) then
      allocate(this%items(max(beam,8)))
      this%n = 0
    end if

    if (this%n < size(this%items)) then
      this%n = this%n + 1
      this%items(this%n) = it
      inserted = .true.
    else
      ! lleno: si no mejora al peor, no entra
      if (it%score <= this%items(this%n)%score) return
      this%items(this%n) = it
      inserted = .true.
    end if

    ! ordenar por score desc (inserción simple; beam chico)
    do i=2,this%n
      if (this%items(i)%score > this%items(i-1)%score) then
        call swap_items(this%items(i), this%items(i-1))
      end if
    end do
    ! garantizar beam
    if (this%n > beam) this%n = beam
  end function

  subroutine swap_items(a,b)
    type(item_t), intent(inout) :: a,b
    type(item_t) :: t
    t=a; a=b; b=t
  end subroutine

  ! -------------------------
  ! JSON loading
  ! -------------------------
  logical function parser_load_resources(this, grammar_path, lexicon_path, beam) result(ok)
    class(parser_t), intent(inout) :: this
    character(len=*), intent(in) :: grammar_path, lexicon_path
    integer, intent(in) :: beam

    type(json_file) :: g, l
    type(json_core) :: core
    logical :: found
    integer(json_ik) :: n, i
    character(len=:), allocatable :: sstart

    ok = .false.
    this%beam = max(1, beam)

    call g%initialize()
    call g%load_file(filename=trim(grammar_path))
    if (g%failed()) return

    call l%initialize()
    call l%load_file(filename=trim(lexicon_path))
    if (l%failed()) return

    ! Start symbol
    call g%get("start", sstart, found)
    if (.not. found) then
      ! tolerancia: "root" o "start_symbol"
      call g%get("root", sstart, found)
      if (.not. found) call g%get("start_symbol", sstart, found)
    end if
    if (.not. found) sstart = "S"
    this%start_sym = this%sym%intern(trim(sstart))

    ! Lexicon entries: entries / lexicon
    call l%info("entries", found=found, n_children=n)
    if (.not. found) call l%info("lexicon", found=found, n_children=n)
    if (.not. found) then
      n = 0
    end if

    this%nlex = int(n, I4)
    if (this%nlex > 0) allocate(this%lex(this%nlex))

    call l%get_core(core)

    do i=1,n
      call read_lex_entry(this, l, core, i)
    end do

    ! Grammar rules: rules / productions
    call g%info("rules", found=found, n_children=n)
    if (.not. found) call g%info("productions", found=found, n_children=n)
    if (.not. found) then
      n = 0
    end if

    this%nrules = int(n, I4)
    if (this%nrules > 0) allocate(this%rules(this%nrules))

    call g%get_core(core)
    do i=1,n
      call read_rule(this, g, core, i)
    end do

    ok = .true.
  end function

  subroutine read_lex_entry(p, jf, core, i)
    class(parser_t), intent(inout) :: p
    type(json_file), intent(inout) :: jf
    type(json_core), intent(inout) :: core
    integer(json_ik), intent(in) :: i

    character(len=:), allocatable :: w, pos
    real(json_rk) :: ww
    logical :: found
    type(json_value), pointer :: feats_obj, child
    integer(json_ik) :: nf, j
    character(len=:), allocatable :: key, val
    character(len=64) :: path

    write(path,'(a,i0,a)') "entries(", i, ").word"
    call jf%get(trim(path), w, found)
    if (.not. found) then
      write(path,'(a,i0,a)') "entries(", i, ").form"
      call jf%get(trim(path), w, found)
    end if
    if (.not. found) w = ""

    write(path,'(a,i0,a)') "entries(", i, ").pos"
    call jf%get(trim(path), pos, found)
    if (.not. found) then
      write(path,'(a,i0,a)') "entries(", i, ").tag"
      call jf%get(trim(path), pos, found)
    end if
    if (.not. found) pos = "X"

    write(path,'(a,i0,a)') "entries(", i, ").weight"
    call jf%get(trim(path), ww, found)
    if (.not. found) then
      write(path,'(a,i0,a)') "entries(", i, ").prob"
      call jf%get(trim(path), ww, found)
    end if
    if (.not. found) ww = 1.0_json_rk

    p%lex(i)%word = trim(w)
    p%lex(i)%pos  = p%sym%intern(trim(pos))
    p%lex(i)%w    = real(max(ww, 1.0e-12_json_rk), real64)
    call p%lex(i)%feats%clear()

    ! feats object: feats / features
    feats_obj => null()
    write(path,'(a,i0,a)') "entries(", i, ").feats"
    call jf%get(trim(path), feats_obj, found)
    if (.not. found) then
      write(path,'(a,i0,a)') "entries(", i, ").features"
      call jf%get(trim(path), feats_obj, found)
    end if
    if (.not. found) return

    call core%info(feats_obj, n_children=nf)
    if (nf <= 0) return

    do j=1,nf
      child => null()
      call core%get_child(feats_obj, j, child, found)
      if (.not. found) cycle
      call core%info(child, name=key)
      call core%get(child, val, found)
      if (.not. found) cycle
      call p%lex(i)%feats%put(p%sym%intern(trim(key)), p%sym%intern(trim(val)))
    end do
    call p%lex(i)%feats%sort()
  end subroutine

  subroutine read_rule(p, jf, core, i)
    class(parser_t), intent(inout) :: p
    type(json_file), intent(inout) :: jf
    type(json_core), intent(inout) :: core
    integer(json_ik), intent(in) :: i

    logical :: found
    character(len=:), allocatable :: lhs, prop
    real(json_rk) :: ww
    integer(json_ik) :: nrhs, nc, j
    character(len=:), allocatable :: rhs_vec(:)
    type(json_value), pointer :: cs_arr, cobj
    character(len=128) :: path

    write(path,'(a,i0,a)') "rules(", i, ").lhs"
    call jf%get(trim(path), lhs, found)
    if (.not. found) then
      write(path,'(a,i0,a)') "productions(", i, ").lhs"
      call jf%get(trim(path), lhs, found)
    end if
    if (.not. found) lhs = "<?>"
    p%rules(i)%lhs = p%sym%intern(trim(lhs))

    write(path,'(a,i0,a)') "rules(", i, ").weight"
    call jf%get(trim(path), ww, found)
    if (.not. found) then
      write(path,'(a,i0,a)') "rules(", i, ").prob"
      call jf%get(trim(path), ww, found)
    end if
    if (.not. found) ww = 1.0_json_rk
    p%rules(i)%w = real(max(ww, 1.0e-12_json_rk), real64)

    write(path,'(a,i0,a)') "rules(", i, ").propagate"
    call jf%get(trim(path), prop, found)
    if (.not. found) prop = "merge"
    p%rules(i)%propagate = trim(prop)

    ! rhs as string vec
    rhs_vec = [character(len=1)::]
    write(path,'(a,i0,a)') "rules(", i, ").rhs"
    call jf%get(trim(path), rhs_vec, found)
    if (.not. found) then
      write(path,'(a,i0,a)') "rules(", i, ").rhs_symbols"
      call jf%get(trim(path), rhs_vec, found)
    end if

    if (found) then
      nrhs = size(rhs_vec, kind=json_ik)
      p%rules(i)%rhs_len = int(nrhs, I4)
      if (nrhs >= 1) p%rules(i)%rhs1 = p%sym%intern(trim(rhs_vec(1)))
      if (nrhs >= 2) p%rules(i)%rhs2 = p%sym%intern(trim(rhs_vec(2)))
    else
      ! tolerancia: rhs1/rhs2
      write(path,'(a,i0,a)') "rules(", i, ").rhs1"
      call jf%get(trim(path), lhs, found)
      if (found) p%rules(i)%rhs1 = p%sym%intern(trim(lhs))
      write(path,'(a,i0,a)') "rules(", i, ").rhs2"
      call jf%get(trim(path), lhs, found)
      if (found) then
        p%rules(i)%rhs2 = p%sym%intern(trim(lhs))
        p%rules(i)%rhs_len = 2
      else
        p%rules(i)%rhs_len = 1
      end if
    end if

    ! constraints array: constraints / conds
    cs_arr => null()
    write(path,'(a,i0,a)') "rules(", i, ").constraints"
    call jf%get(trim(path), cs_arr, found)
    if (.not. found) then
      write(path,'(a,i0,a)') "rules(", i, ").conds"
      call jf%get(trim(path), cs_arr, found)
    end if
    if (.not. found) then
      p%rules(i)%ncs = 0
      return
    end if

    call core%info(cs_arr, n_children=nc)
    p%rules(i)%ncs = int(max(nc,0_json_ik), I4)
    if (p%rules(i)%ncs <= istani(0)) return
    allocate(p%rules(i)%cs(p%rules(i)%ncs))

    do j=1,nc
      cobj => null()
      call core%get_child(cs_arr, j, cobj, found)
      if (.not. found) cycle
      call read_constraint(p, core, cobj, p%rules(i)%cs(j))
    end do
  contains
    integer(I4) function istani(x) result(r)
      integer, intent(in) :: x
      r = int(x, I4)
    end function
  end subroutine

  subroutine read_constraint(p, core, obj, c)
    class(parser_t), intent(inout) :: p
    type(json_core), intent(inout) :: core
    type(json_value), pointer, intent(in) :: obj
    type(constraint_t), intent(inout) :: c

    logical :: found
    character(len=:), allocatable :: s, t, t2, k, v, k2

    call core%get(obj, "type", s, found)
    if (.not. found) s = "require"
    c%ctype = trim(s)

    call core%get(obj, "target", t, found)
    if (.not. found) t = "left"
    c%target = trim(t)

    call core%get(obj, "target2", t2, found)
    if (.not. found) then
      call core%get(obj, "other", t2, found)
      if (.not. found) t2 = "right"
    end if
    c%target2 = trim(t2)

    call core%get(obj, "key", k, found)
    if (.not. found) k = ""
    c%key = p%sym%intern(trim(k))

    call core%get(obj, "value", v, found)
    if (.not. found) v = ""
    c%val = p%sym%intern(trim(v))

    call core%get(obj, "key2", k2, found)
    if (.not. found) k2 = ""
    c%key2 = p%sym%intern(trim(k2))
  end subroutine

  ! -------------------------
  ! Parsing
  ! -------------------------
  function parser_parse_sentence(this, sentence) result(tree)
    class(parser_t), intent(inout) :: this
    character(len=*), intent(in) :: sentence
    character(len=:), allocatable :: tree

    character(len=:), allocatable :: toks0(:), toks(:)
    integer(I4) :: n0, n
    integer(I4) :: i, j, k, span
    type(cell_t), allocatable :: chart(:,:)
    type(item_t) :: it
    integer(I4) :: best_node
    real(real64) :: best_score

    call tokenize(sentence, toks0, n0)
    call split_contractions_and_enclitics(toks0, n0, toks, n)
    if (n <= 0) then
      tree = ""
      return
    end if

    call arena_reset(this)

    allocate(chart(n, n+1))
    do i=1,n
      call init_cell(chart(i,i+1))
      call seed_lexical(this, chart(i,i+1), toks(i))
      call unary_closure(this, chart(i,i+1))
    end do

    do span = 2, n
      do i=1, n-span+1
        j = i + span
        call init_cell(chart(i,j))

        do k = i+1, j-1
          call combine_cells(this, chart(i,k), chart(k,j), chart(i,j))
        end do

        call unary_closure(this, chart(i,j))
      end do
    end do

    call pick_best_root(this, chart(1,n+1), best_node, best_score)
    if (best_node <= 0) then
      tree = ""
    else
      tree = render_tree(this, best_node)
    end if
  end function

  subroutine init_cell(c)
    type(cell_t), intent(inout) :: c
    c%n = 0
    if (allocated(c%cats)) deallocate(c%cats)
    if (allocated(c%bucks)) deallocate(c%bucks)
  end subroutine

  subroutine arena_reset(p)
    class(parser_t), intent(inout) :: p
    p%narena = 0
    if (allocated(p%arena)) deallocate(p%arena)
    allocate(p%arena(256))
  end subroutine

  integer(I4) function arena_add(p, nd) result(id)
    class(parser_t), intent(inout) :: p
    type(node_t), intent(in) :: nd
    type(node_t), allocatable :: b(:)
    integer :: newcap
    if (.not. allocated(p%arena)) then
      allocate(p%arena(256))
      p%narena = 0
    end if
    if (p%narena+1 > size(p%arena)) then
      newcap = 2*size(p%arena)
      allocate(b(newcap))
      b(1:p%narena) = p%arena(1:p%narena)
      call move_alloc(b, p%arena)
    end if
    p%narena = p%narena + 1
    p%arena(p%narena) = nd
    id = p%narena
  end function

  subroutine seed_lexical(p, cell, word)
    class(parser_t), intent(inout) :: p
    type(cell_t), intent(inout) :: cell
    character(len=*), intent(in) :: word

    integer :: i
    logical :: any
    type(item_t) :: it
    type(node_t) :: nd
    character(len=:), allocatable :: wl, lw

    any = .false.
    lw = trim(word)
    wl = lower_ascii(lw)

    do i=1,p%nlex
      if (trim(lower_ascii(p%lex(i)%word)) == trim(wl)) then
        any = .true.
        it%cat = p%lex(i)%pos
        it%feats = p%lex(i)%feats
        it%fh = it%feats%hash64()
        it%score = log(p%lex(i)%w)
        nd%label = it%cat
        nd%is_leaf = .true.
        nd%leaf = lw
        it%node_id = arena_add(p, nd)
        call cell%add_item(it, p%beam)
      end if
    end do

    if (.not. any) then
      ! fallback OOV: PROPN si inicial mayúscula ASCII, si no NOUN
      if (len_trim(lw) > 0 .and. iachar(lw(1:1)) >= iachar('A') .and. iachar(lw(1:1)) <= iachar('Z')) then
        it%cat = p%sym%intern("PROPN")
      else
        it%cat = p%sym%intern("NOUN")
      end if
      call it%feats%clear()
      it%fh = it%feats%hash64()
      it%score = log(1.0e-6_real64)
      nd%label = it%cat
      nd%is_leaf = .true.
      nd%leaf = lw
      it%node_id = arena_add(p, nd)
      call cell%add_item(it, p%beam)
    end if
  end subroutine

  subroutine unary_closure(p, cell)
    class(parser_t), intent(inout) :: p
    type(cell_t), intent(inout) :: cell

    logical :: changed
    integer :: iter, b, ii, r
    type(item_t) :: src, out
    type(node_t) :: nd

    changed = .true.
    iter = 0
    do while (changed .and. iter < 64)
      changed = .false.
      iter = iter + 1

      do b=1,cell%n
        do ii=1,cell%bucks(b)%n
          src = cell%bucks(b)%items(ii)
          do r=1,p%nrules
            if (p%rules(r)%rhs_len /= 1) cycle
            if (p%rules(r)%rhs1 /= src%cat) cycle

            if (.not. apply_constraints_unary(p, p%rules(r), src%feats, out%feats)) cycle
            out%cat = p%rules(r)%lhs
            call out%feats%sort()
            out%fh = out%feats%hash64()
            out%score = src%score + log(p%rules(r)%w)

            nd%label = out%cat
            nd%left = src%node_id
            nd%right = 0
            nd%is_leaf = .false.
            if (allocated(nd%leaf)) deallocate(nd%leaf)
            out%node_id = arena_add(p, nd)

            if (cell%add_item(out, p%beam)) changed = .true.
          end do
        end do
      end do
    end do
  end subroutine

  logical function apply_constraints_unary(p, rule, child_feats, parent_feats) result(ok)
    class(parser_t), intent(inout) :: p
    type(rule_t), intent(in) :: rule
    type(featlist_t), intent(in) :: child_feats
    type(featlist_t), intent(inout) :: parent_feats

    integer :: c
    ok = .true.
    parent_feats = child_feats

    if (rule%ncs <= 0) return

    do c=1,rule%ncs
      select case (trim(rule%cs(c)%ctype))
      case ("require")
        if (child_feats%get(rule%cs(c)%key) /= rule%cs(c)%val) then
          ok = .false.; return
        end if
      case ("assign")
        call parent_feats%put(rule%cs(c)%key, rule%cs(c)%val)
      case default
        cycle
      end select
    end do
  end function

  subroutine combine_cells(p, left, right, outcell)
    class(parser_t), intent(inout) :: p
    type(cell_t), intent(inout) :: left, right, outcell

    integer :: bl, br, il, ir, r
    type(item_t) :: L, R, O
    type(node_t) :: nd

    do bl=1,left%n
      do il=1,left%bucks(bl)%n
        L = left%bucks(bl)%items(il)
        do br=1,right%n
          do ir=1,right%bucks(br)%n
            R = right%bucks(br)%items(ir)

            do r=1,p%nrules
              if (p%rules(r)%rhs_len /= 2) cycle
              if (p%rules(r)%rhs1 /= L%cat) cycle
              if (p%rules(r)%rhs2 /= R%cat) cycle

              if (.not. apply_constraints_binary(p, p%rules(r), L%feats, R%feats, O%feats)) cycle

              O%cat = p%rules(r)%lhs
              call O%feats%sort()
              O%fh = O%feats%hash64()
              O%score = L%score + R%score + log(p%rules(r)%w)

              nd%label = O%cat
              nd%left = L%node_id
              nd%right = R%node_id
              nd%is_leaf = .false.
              if (allocated(nd%leaf)) deallocate(nd%leaf)
              O%node_id = arena_add(p, nd)

              call outcell%add_item(O, p%beam)
            end do
          end do
        end do
      end do
    end do
  end subroutine

  logical function apply_constraints_binary(p, rule, lf, rf, pf) result(ok)
    class(parser_t), intent(inout) :: p
    type(rule_t), intent(in) :: rule
    type(featlist_t), intent(in) :: lf, rf
    type(featlist_t), intent(inout) :: pf

    integer :: c
    integer(I4) :: v1, v2, k
    ok = .true.
    call pf%clear()

    select case (trim(rule%propagate))
    case ("left")
      pf = lf
    case ("right")
      pf = rf
    case default
      pf = lf
      call merge_feats(pf, rf)
    end select

    if (rule%ncs <= 0) return

    do c=1,rule%ncs
      select case (trim(rule%cs(c)%ctype))
      case ("require")
        if (trim(rule%cs(c)%target) == "left") then
          if (lf%get(rule%cs(c)%key) /= rule%cs(c)%val) then
            ok = .false.; return
          end if
        else
          if (rf%get(rule%cs(c)%key) /= rule%cs(c)%val) then
            ok = .false.; return
          end if
        end if

      case ("unify")
        k  = rule%cs(c)%key
        v1 = lf%get(k)
        v2 = rf%get(k)
        if (v1 /= 0 .and. v2 /= 0 .and. v1 /= v2) then
          ok = .false.; return
        else if (v1 /= 0) then
          call pf%put(k, v1)
        else if (v2 /= 0) then
          call pf%put(k, v2)
        end if

      case ("agree")
        k  = rule%cs(c)%key
        v1 = lf%get(k)
        v2 = rf%get(k)
        if (v1 == 0 .or. v2 == 0 .or. v1 /= v2) then
          ok = .false.; return
        end if
        call pf%put(k, v1)

      case ("assign")
        call pf%put(rule%cs(c)%key, rule%cs(c)%val)

      case default
        cycle
      end select
    end do
  end function

  subroutine merge_feats(dst, src)
    type(featlist_t), intent(inout) :: dst
    type(featlist_t), intent(in) :: src
    integer :: i
    do i=1,src%n
      if (dst%get(src%a(i)%k) == 0) call dst%put(src%a(i)%k, src%a(i)%v)
    end do
  end subroutine

  subroutine pick_best_root(p, cell, best_node, best_score)
    class(parser_t), intent(in) :: p
    type(cell_t), intent(in) :: cell
    integer(I4), intent(out) :: best_node
    real(real64), intent(out) :: best_score
    integer :: b, i
    best_node = 0
    best_score = -1.0e300_real64
    do b=1,cell%n
      if (cell%cats(b) /= p%start_sym) cycle
      do i=1,cell%bucks(b)%n
        if (cell%bucks(b)%items(i)%score > best_score) then
          best_score = cell%bucks(b)%items(i)%score
          best_node  = cell%bucks(b)%items(i)%node_id
        end if
      end do
    end do
  end subroutine

  recursive function render_tree(p, node_id) result(s)
    class(parser_t), intent(in) :: p
    integer(I4), intent(in) :: node_id
    character(len=:), allocatable :: s
    character(len=:), allocatable :: lab, a, b

    if (node_id <= 0 .or. node_id > p%narena) then
      s = ""
      return
    end if

    lab = p%sym%str(p%arena(node_id)%label)

    if (p%arena(node_id)%is_leaf) then
      s = "(" // lab // " " // trim(p%arena(node_id)%leaf) // ")"
    else if (p%arena(node_id)%right == 0) then
      a = render_tree(p, p%arena(node_id)%left)
      s = "(" // lab // " " // a // ")"
    else
      a = render_tree(p, p%arena(node_id)%left)
      b = render_tree(p, p%arena(node_id)%right)
      s = "(" // lab // " " // a // " " // b // ")"
    end if
  end function

end module nlp_parser_v7
