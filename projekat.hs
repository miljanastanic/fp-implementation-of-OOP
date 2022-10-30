import Text.Parsec
import System.Environment
import System.IO
import Text.Parsec.Char



data Class = RegularClass { classtype :: String, classname :: String, package :: String, parent :: String, field :: [Field], 
                    relationship:: [Relationship], method :: [Method], constructor :: [String] } 
               | AbstractClass {classtype :: String, classname :: String, package :: String, field :: [Field],
                    method :: [Method], constructor :: [String]}
               | Interface {classtype :: String, classname :: String, package :: String, method :: [Method]}     

data Field = Field { fieldname :: String, fieldtype :: String, iscollection :: String, methods :: [String]
                    --,primary :: String
                     }

data Method = Method { returntype :: String, methodname :: String, arguments:: [String] }

data Relationship = RelationshipOwner { relationshipname :: String, cardinality :: String, relationshipwith :: String, methodsr :: [String], owner:: String} 
                    | RelationshipNotOwner {relationshipname :: String, cardinality :: String, relationshipwith :: String, methodsr :: [String]} 

data Object = Object Class [Field] Relationship | Empty deriving Show


instance Show Class where
     show (RegularClass classtype classname package parent fields relationship method constructor) =
          "\npackage " ++ package ++ ";" ++ "\nimport lombok.*;\n" ++ constructorHelp constructor ++ 
          "\n" ++ "public class " ++ classname ++ 
          --" extends " ++ parent ++ 
          (if length parent > 1 then " extends " ++ parent ++ " {\n\n" else " {\n\n") ++
          fieldHelper fields ++ fieldHelper relationship  ++ methodHelper method ++ "{\n\t\t\tno implementation\n\t }" ++ "\n}\n\n"
          where fieldHelper [] = ""
                fieldHelper (x:xs) = show x ++ "\n" ++ fieldHelper xs
                methodHelper [] = ""
                methodHelper (x:xs) = "\n\n\t private" ++ show x ++ methodHelper xs
                constructorHelp [] = ""
                constructorHelp (x:xs) = "\n@" ++ x ++ constructorHelp xs

     show (AbstractClass classtype classname package fields method constructor) =
          "\npackage " ++ package ++ ";" ++ "\nimport lombok.*;\n" ++ constructorHelp constructor ++ 
          "\n" ++ "public abstract class " ++ classname ++ " {\n\n" ++ 
          fieldHelper fields ++ methodHelper method ++ "{\n\t\t\tno implementation\n\t }" ++ "\n}\n\n"
          where fieldHelper [] = ""
                fieldHelper (x:xs) = show x ++ "\n" ++ fieldHelper xs
                methodHelper [] = ""
                methodHelper (x:xs) = "\n\n\tprivate" ++ show x ++ methodHelper xs
                constructorHelp [] = ""
                constructorHelp (x:xs) = "\n@" ++ x ++ constructorHelp xs

     show (Interface classtype classname package method) = 
          "\npackage " ++ package ++ ";" ++ "\n" ++ "public interface " ++ classname ++ " {\n" ++ "\t" ++
          methodHelper method ++ ";" ++ "\n}\n\n"
          where methodHelper [] = ""
                methodHelper (x:xs) = "\tprivate" ++ show x ++ methodHelper xs 

instance Show Field where
     show (Field fieldname fieldtype iscollection methods) = 
          "\t" ++ methodsHelp methods ++ "\n\t private " ++ 
          --(if length iscollection < 0 then "List <" ++ fieldtype ++ ">" else fieldtype) ++ " " ++ fieldname ++ ";"
          case iscollection of  " *" -> "List<" ++ fieldtype ++ ">" ++ " " ++ fieldname ++ ";"
                                " " -> fieldtype ++ " " ++ fieldname ++ ";"
          where methodsHelp [] = ""
                methodsHelp (x:xs) = "@" ++ x ++ " " ++ methodsHelp xs


instance Show Relationship where
     show (RelationshipOwner relationshipname cardinality relationshipwith methodsr owner) = 
          "\t" ++ "@" ++ cardinality ++ owner ++ " " ++ methodsH methodsr ++
          "\n\t private " ++ relationshipwith ++ " " ++ relationshipname 
          where methodsH [] = ""
                methodsH (x:xs) = "@" ++ x ++ " " ++ methodsH xs
     
     show (RelationshipNotOwner relationshipname cardinality relationshipwith methodsr ) = 
          "\t" ++ "@" ++ cardinality ++ " " ++ methodsH methodsr ++
          "\n\t private " ++ relationshipwith ++ " " ++ relationshipname 
          where methodsH [] = ""
                methodsH (x:xs) = "@" ++ x ++ " " ++ methodsH xs


instance Show Method where
     show (Method returntype methodname arguments) = 
          " " ++ returntype ++ " " ++ methodname ++ " (" ++ argsHelper arguments ++ ")"
          where argsHelper [] = ""
                argsHelper (x:xs) = x ++ " ime" ++ argsHelper xs



classParser :: Parsec String Int Class
classParser = do 
     manyTill anyChar (try (string "#"))
     classtype <- manyTill letter space
     spaces
     classname <- manyTill letter space
     manyTill anyChar (try (string ":"))
     package <- manyTill letter space
     
     case classtype of "c"-> do
                         --manyTill anyChar (try (string "-"))
                         parent <- manyTill anyChar (try (string "\n"))
                         field <- many (try fieldParser)
                         spaces
                         relationship <- many (try relationshipParser)
                         spaces
                         method <- many (try methodParser)
                         spaces
                         constructor <- (try constructorParser)
                         spaces
                         return (RegularClass {classtype=classtype, classname=classname, package=package, parent=parent, field=field, 
                                        relationship=relationship, method=method, constructor=constructor})

                       "a" -> do
                         spaces
                         field <- many (try fieldParser)
                         spaces
                         method <- many (try methodParser)
                         spaces
                         constructor <- (try constructorParser)
                         spaces
                         return (AbstractClass {classtype=classtype, classname=classname, package=package, field=field, 
                                        method=method, constructor=constructor})

                       "i" -> do
                         spaces
                         method <- many (try methodParser)
                         spaces
                         return (Interface {classtype=classtype, classname=classname, package=package, method=method})


fieldParser :: Parsec String Int Field
fieldParser = do
     fieldname <- manyTill letter space
     spaces
     fieldtype <- many1 letter
     iscollection <- ((try(string " *")) <|> string " ")
     methods <- many (try methodsHelper)
     spaces
     return (Field {fieldname=fieldname, fieldtype=fieldtype, iscollection=iscollection, methods=methods})


methodsHelper :: Parsec String Int String
methodsHelper = do
     spaces
     string "@"
     many1 letter
     --string "get" <|> string "set" <|> string "add"


relationshipParser :: Parsec String Int Relationship
relationshipParser = do
     relationshipname <- many letter
     spaces
     string "@"
     cardinality <- many letter
     spaces
     relationshipwith <- many letter
     owner <- ((try(string " @owner")) <|> string " ")
     methodsr <- many (try constructorHelper)
     spaces
     case owner of 
                    " @owner" ->  return (RelationshipOwner {relationshipname=relationshipname, cardinality=cardinality, relationshipwith=relationshipwith, owner = owner, methodsr=methodsr})
                    
                    " " ->  return (RelationshipNotOwner {relationshipname=relationshipname, cardinality=cardinality, relationshipwith=relationshipwith, methodsr=methodsr})

methodParser :: Parsec String Int Method
methodParser = do 
     string "method :"
     returntype <- manyTill letter space
     methodname <- manyTill letter space
     spaces
     string "$args"
     arguments <- many (try argumentsHelper)
     return (Method {returntype=returntype, methodname=methodname, arguments=arguments})


argumentsHelper :: Parsec String Int String
argumentsHelper = do
     spaces
     string "$"
     many1 letter


constructorParser :: Parsec String Int [String]
constructorParser = do
     string "constructor"
     constructorTypes <- many (try constructorHelper)
     spaces
     return constructorTypes


constructorHelper :: Parsec String Int String
constructorHelper = do
     spaces
     string "@"
     many letter

parsing :: Parsec String Int [Class]
parsing = do
     spaces
     clas <- many (classParser)
     return clas


fun :: String -> Bool
fun x = case x of 
                "int" -> True
                "char" -> True
                "boolean" -> True
                "float" -> True
                otherwise -> False


main :: IO ()
main = do 
          let input = "projekat/klasniDodatno.txt"
              output = "projekat/test.txt"
          h <- openFile input ReadMode
          hSetEncoding h latin1
          cnts <- hGetContents h
          case (runParser parsing 1 input cnts) of  -- parse
            Left err -> putStrLn . show $ err
            Right classParser -> writeFile output . show $ classParser  -- putStrLn . show $ rss     




