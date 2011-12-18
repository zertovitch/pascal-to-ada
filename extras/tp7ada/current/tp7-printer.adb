---------------------------------------------------------------------------
-- NOM DU CSU (corps)               : tp7-printer.adb
-- AUTEUR DU CSU                    : Pascal Pignard
-- VERSION DU CSU                   : 2.1a
-- DATE DE LA DERNIERE MISE A JOUR  : 16 octobre 2011
-- ROLE DU CSU                      : Unité d'émulation Turbo Pascal 7.0.
--
--
-- FONCTIONS EXPORTEES DU CSU       :
--
-- FONCTIONS LOCALES DU CSU         :
--
--
-- NOTES                            :
--
-- COPYRIGHT                        : (c) Pascal Pignard 2002-2011
-- LICENCE                          : CeCILL V2 (http://www.cecill.info)
-- CONTACT                          : http://blady.pagesperso-orange.fr
-------------------------------------------------------------------------------

with Ada.Text_IO;

package body TP7.Printer is
begin
   Lst.Device := File_System;
   Ada.Text_IO.Create (Lst.File, Ada.Text_IO.Out_File, "LPT1.txt");
end TP7.Printer;
