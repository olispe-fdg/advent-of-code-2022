with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Handling;

with Ada.Command_Line;

with Types; use Types;

procedure Advent_Of_Code is
    package IO renames Ada.Text_IO;
    package Unbounded_IO renames Ada.Text_IO.Unbounded_IO;
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

    Elf_A : Elf;
    Elf_B : Elf;
begin
    IO.Put_Line ("Reading file: " & File_Name);
    IO.Open (File, IO.In_File, File_Name);

    while not IO.End_Of_File (File) loop
        declare
            Line : String := IO.Get_Line (File);
        begin
            Parse_Elves (Line, Elf_A, Elf_B);

            IO.Put_Line
               ("Elf A: " & Range_Bound'Image (Elf_A.Range_Start) & ", " &
                Range_Bound'Image (Elf_A.Range_End));
            IO.Put_Line
               ("Elf B: " & Range_Bound'Image (Elf_B.Range_Start) & ", " &
                Range_Bound'Image (Elf_B.Range_End));
        end;
    end loop;
end Advent_Of_Code;
