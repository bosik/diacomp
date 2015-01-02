<?php
    require_once 'safe_slash.php';
    require_once 'mysql.php';
	require_once 'common.php'; // is it actually required? added for STD_TIME_FMT
    require_once 'printer.php';
    
    //require_once 'common.php'; // DEBUG
    
    if (empty($_GET) || !isset($_GET['key']))
    {        
        header('Location: index.php');
    }
    else
    {
        $query = "SELECT `Login`, `HashPass`" .
                 "FROM `Activate` WHERE `Key` = '" . $_GET['key'] . "'";
        $sql = mysqli_query(sqlLink(), $query);
        if (mysqli_num_rows($sql) == 1)
        {
            $row = mysqli_fetch_assoc($sql);
            $login = $row['Login'];
            $hashpass = $row['HashPass'];
            
            // добавляем данные в таблицу User
            $query = "INSERT INTO `User`(`Login`, `HashPass`, `DateReg`)" .
                     "VALUES ('{$login}', '{$hashpass}', '" . 
                          gmdate(STD_TIME_FMT) ."')";
            mysqli_query(sqlLink(), $query);
            
            // убиваем информацию об активации
            $query = "DELETE FROM `Activate` WHERE `Login`='{$login}'";
            mysqli_query(sqlLink(), $query);
            
            // информируем
            header('Refresh: 2; URL=index.php');
            PrintActivationSucceed();            
        }
    }
?>
