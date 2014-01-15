-------------------------------------------------------------------------------
-- NOM DU CSU (spécification)       : tp7-test.ads
-- AUTEUR DU CSU                    : Pascal Pignard
-- VERSION DU CSU                   : 1.0a
-- DATE DE LA DERNIERE MISE A JOUR  : 16 décembre 2011
-- ROLE DU CSU                      : Unité de test.
--
--
-- FONCTIONS EXPORTEES DU CSU       :
--
-- FONCTIONS LOCALES DU CSU         :
--
--
-- NOTES                            :
--
-- COPYRIGHT                        : (c) Pascal Pignard 2011
-- LICENCE                          : CeCILL V2 (http://www.cecill.info)
-- CONTACT                          : http://blady.pagesperso-orange.fr
-------------------------------------------------------------------------------

package TP7.Test is

   -- Provide a list of procedures which can be selected by user with a check box
   -- The marked procedure are executed on demand

   -- Chain the given procedure in execute list
   procedure Add (TestProc : TPProc; TestName : TPString);
   -- Mark all chained procedures to be executed
   procedure SelectAll;
   -- Execute all marked procedures in excute list
   procedure Execute;

end TP7.Test;
