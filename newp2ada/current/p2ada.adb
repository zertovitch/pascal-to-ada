-- $Id: p2ada.adb,v 2.1 1997/08/23 19:13:14 nestor Rel $
-- $Locker:  $
-- $Log: p2ada.adb,v $
-- Revision 2.1  1997/08/23 19:13:14  nestor
-- Laurent Gasser's correction for MacOS extended Pascal
--
-- Revision 1.1  1997/08/23  07:13:25  nestor
-- Martin C. Carlisle's original version, standard Pascal
--
with YYParse;

-- p2ada
--
-- Martin C. Carlisle, US Air Force Academy
-- November 26, 1996
--

-- Craig Carey 15-Nov-2002:

--  Allow reading from a file to permit debugging for when the debugger is
--  is not reading from the standard input or writing to the standard
--  output.

with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Text_IO;                       use Ada.Text_IO;
with Pascal_IO;
with P2Ada_Definition_info;

procedure P2Ada is
   Inp_Opened  : Boolean := False;
   Out_Opened  : Boolean := False;

  procedure Syntax is
  begin
    Put_Line( Standard_Error, "Syntax: P2Ada [input_file] {-Iimports} [-Eexports] [-ooutput_file]" );
    New_Line( Standard_Error );
    Put_Line( Standard_Error, "The file 'exports' is produced with the definitions of:" );
    Put_Line( Standard_Error, "  - the 'interface' part of Borland/UCSD Pascal 'unit'" );
    Put_Line( Standard_Error, "  - the 'export' part of an ISO Pascal 'module' (not yet implemented)" );
    Put_Line( Standard_Error, "  - the top-level definitions of a program, considered as a code snippet." );
  end Syntax;

begin
  Put_Line (Standard_Error, "Welcome to P2Ada, the Pascal to Ada translator. Type ""p2ada -h"" for help.");
  P2Ada_Definition_info.Load_Alias ("alias.txt");

  for i in 1..Argument_Count loop
    declare
      arg: constant String:= Argument(i);
      u_arg: constant String:= To_Upper( arg );
    begin
      if u_arg'length > 1 and then
        (u_arg(1) = '-' or u_arg(1) = '/') then
        case u_arg(2) is
          when 'I' =>
            begin
              P2Ada_Definition_info.Load( arg(3..arg'last) );
            exception
              when Name_Error =>
                Syntax;
                return;
            end;
          when 'E' =>
            P2Ada_Definition_info.Will_save( arg(3..arg'last) );
          when 'O' =>
            if Out_Opened then          -- Two outputs ?!
              Syntax;
              return;
            else
              declare
                out_name: constant String:= arg(3..arg'last);
              begin
                pascal_io.Create_Output(fname => out_name);
                Out_Opened := True;
                P2Ada_Definition_info.Set_Ada_output(out_name);
              exception
                when Name_Error =>
                  Put_Line( Standard_Error, "Cannot write Output file '" & out_name & "'." );
                  Syntax;
                  return;
              end;
            end if;
          when others =>  -- includes "-h"
            Syntax;
            return;
        end case;
      else -- no option
        if Inp_Opened then          -- Two inputs ?!
          pascal_io.Close_Input;
          Syntax;
          return;
        else
          begin
            pascal_io.Open_Input (fname => arg);
            Inp_Opened := True;
            P2Ada_Definition_info.Set_Pascal_source(arg);
          exception
            when Name_Error =>
              Put_Line( Standard_Error, "Input file '" & arg & "' not found." );
              Syntax;
              return;
          end;
        end if;
      end if;
    end;
   end loop;
   --  Otherwise pascal_io.user_input_file is not initialized but
   --  it runs OK and equals 0 or null.

   Put(Standard_error, "P2Ada from ");
   if Inp_opened then
     Put(Standard_error, ''' & P2Ada_Definition_info.Get_Pascal_source & ''');
   else
     Put(Standard_error, "Standard_Input");
   end if;
   Put(Standard_error, " to ");
   if Out_opened then
     Put(Standard_error, ''' & P2Ada_Definition_info.Get_Ada_output & ''');
   else
     Put(Standard_error, "Standard_Output");
   end if;
   New_Line(Standard_error);

   YYParse;

   if Inp_Opened then
     pascal_io.Close_Input;
   end if;
   if Out_Opened then
     pascal_io.Close_Output;
   end if;

end p2ada;
