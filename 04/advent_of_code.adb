with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.Command_Line;

procedure Advent_Of_Code is
    package IO renames Ada.Text_IO;

    File_Name : constant String := Ada.Command_Line.Argument (1);
    Line      : String          := "";
    File      : IO.File_Type;
begin
    IO.Put_Line ("Reading file: " & File_Name);
    IO.Open (File => File, Mode => IO.In_File, Name => File_Name);

    while not IO.End_Of_File (File) loop
        IO.Put_Line (Item => IO.Get_Line (File));
    end loop;
end Advent_Of_Code;
