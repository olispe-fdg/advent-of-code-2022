with Ada.Text_IO;
with Ada.Characters.Handling;

with Ada.Command_Line;

with Types; use Types;

procedure Advent_Of_Code is
    package IO renames Ada.Text_IO;
    package Chars renames Ada.Characters.Handling;

    function Scan_Range_Bound
       (Str : in String; Ptr : in out Natural) return Range_Bound
    is
        Ret_Val : Range_Bound;
    begin
        for I in Ptr .. Str'Last loop
            if not Chars.Is_Digit (Str (I)) then
                Ret_Val := Range_Bound'Value (Str (Ptr .. I - 1));
                Ptr     := I + 1;

                return Ret_Val;
            end if;
        end loop;

        Ret_Val := Range_Bound'Value (Str (Ptr .. Str'Last));
        Ptr     := Str'Last;

        return Ret_Val;
    end Scan_Range_Bound;

    function Test_Elf_Overlap (Elf_A : Elf; Elf_B : Elf) return Overlap is
        Min_Elf : Elf;
        Max_Elf : Elf;

        Start_Overlaps : Boolean;
        End_Overlaps   : Boolean;
    begin
        if Elf_A.Range_End - Elf_A.Range_Start >
           Elf_B.Range_End - Elf_B.Range_Start
        then
            Min_Elf := Elf_B;
            Max_Elf := Elf_A;
        else
            Min_Elf := Elf_A;
            Max_Elf := Elf_B;
        end if;

        Start_Overlaps :=
           Min_Elf.Range_Start in Max_Elf.Range_Start .. Max_Elf.Range_End;
        End_Overlaps   :=
           Min_Elf.Range_End in Max_Elf.Range_Start .. Max_Elf.Range_End;

        if Start_Overlaps and End_Overlaps then
            return Overlap_Full;
        elsif Start_Overlaps or End_Overlaps then
            return Overlap_Partial;
        else
            return Overlap_None;
        end if;
    end Test_Elf_Overlap;

    procedure Parse_Elves (Line : in String; Elf_A : out Elf; Elf_B : out Elf)
    is
        type Index is range 1 .. 2;
        type Elf_Array is array (Index) of Elf;

        Elves : Elf_Array;

        Scan_Ptr : Natural := Line'First;
    begin
        for I in Index loop
            Elves (I).Range_Start := Scan_Range_Bound (Line, Scan_Ptr);
            Elves (I).Range_End   := Scan_Range_Bound (Line, Scan_Ptr);
        end loop;

        Elf_A := Elves (1);
        Elf_B := Elves (2);
    end Parse_Elves;

    File_Name : constant String := Ada.Command_Line.Argument (1);
    File      : IO.File_Type;

    Full_Overlap_Pairs    : Natural := 0;
    Partial_Overlap_Pairs : Natural := 0;
begin
    IO.Put_Line ("Reading file: " & File_Name);
    IO.Open (File, IO.In_File, File_Name);

    while not IO.End_Of_File (File) loop
        declare
            Line  : String := IO.Get_Line (File);
            Elf_A : Elf;
            Elf_B : Elf;

            Overlapping : Overlap;
        begin
            Parse_Elves (Line, Elf_A, Elf_B);

            Overlapping := Test_Elf_Overlap (Elf_A, Elf_B);

            if Overlapping /= Overlap_None then
                Partial_Overlap_Pairs := Partial_Overlap_Pairs + 1;

                if Overlapping = Overlap_Full then
                    Full_Overlap_Pairs := Full_Overlap_Pairs + 1;
                end if;
            end if;
        end;
    end loop;

    IO.Put_Line
       ("Fully Overlapping Pairs: " & Natural'Image (Full_Overlap_Pairs));
    IO.Put_Line
       ("Partially Overlapping Pairs: " &
        Natural'Image (Partial_Overlap_Pairs));
end Advent_Of_Code;
