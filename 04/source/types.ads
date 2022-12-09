package Types is
    type Range_Bound is mod 2**32;

    type Elf is record
        Range_Start : Range_Bound;
        Range_End   : Range_Bound;
    end record;

    type Elves is record
        A : Elf;
        B : Elf;
    end record;

    type Overlap is (Overlap_None, Overlap_Partial, Overlap_Full);
end Types;
