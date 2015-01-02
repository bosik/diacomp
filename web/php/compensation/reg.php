<?php
    require_once 'printer.php';
    require_once 'safe_slash.php';
    require_once 'common.php';

    // если это первый запрос
    if (empty($_POST))
    {
        $userinfo = "";
        $menu = BlockMenu(SCREEN_AUTH, 2);
        $content = BlockRegForm("", "", "");
        echo BlockMain($userinfo, $menu, $content);
    }
    else

    // если это попытка регистрации
    {
        // если есть системные ошибки
        if (!isset($_POST['login']) ||
            !isset($_POST['password']) ||
            !isset($_POST['captcha']) ||
            !isset($_SESSION['captcha_keystring'])
            )
        {
            $userinfo = "";
            $menu = BlockMenu(SCREEN_AUTH, 2);
            $content = BlockRegForm("", "", "Повторите попытку");
            echo BlockMain($userinfo, $menu, $content);
        }

        else

        // если системных ошибок нет, проверяем содержание
        {
            $newlogin = $_POST['login'];
            $newpass = $_POST['password'];
            if ($newlogin == '')
            {
                $userinfo = "";
                $menu = BlockMenu(SCREEN_AUTH, 2);
                $content = BlockRegForm("", "", "Введите email");
                echo BlockMain($userinfo, $menu, $content);
            }

            else

            if (ExistsUser($newlogin))
            {
                $userinfo = "";
                $menu = BlockMenu(SCREEN_AUTH, 2);
                $content = BlockRegForm("", "", "Пользователь с таким email уже существует");
                echo BlockMain($userinfo, $menu, $content);
            }

            else

            if (strlen($newpass) < 6)
            {
                $userinfo = "";
                $menu = BlockMenu(SCREEN_AUTH, 2);
                $content = BlockRegForm("", "", "Пароль должен быть не короче шести символов");
                echo BlockMain($userinfo, $menu, $content);
            }

            else

            if ($_POST['captcha'] == '')
            {
                $userinfo = "";
                $menu = BlockMenu(SCREEN_AUTH, 2);
                $content = BlockRegForm("", "", "Введите код с картинки");
                echo BlockMain($userinfo, $menu, $content);
            }

            else

            if (!($_SESSION['captcha_keystring'] == $_POST['captcha']))
            {
                $userinfo = "";
                $menu = BlockMenu(SCREEN_AUTH, 2);
                $content = BlockRegForm("", "", "Введите код ещё раз");
                echo BlockMain($userinfo, $menu, $content);
            }

            else

            // регистрируем!
            {
                require_once 'mysql.php';
                require_once 'common.php';

                $hash = md5($newpass);
                $key = GenerateKey();

                if (!AddActivate($newlogin, $hash, $key))
                {
                    $userinfo = "";
                    $menu = BlockMenu(SCREEN_AUTH, 2);
                    $content = BlockRegForm("", "", "Ошибка регистрации (MySQL)");// . mysqli_error(sqlLink()));
                    echo BlockMain($userinfo, $menu, $content);
                }
                else
                {
                    if (!SendRegMail($newlogin, $key))
                    {
                        $userinfo = "";
                        $menu = BlockMenu(SCREEN_AUTH, 2);
                        $content = BlockRegForm("", "", "Ошибка регистрации (mail)");
                        echo BlockMain($userinfo, $menu, $content);
                    }
                    else
                    {
                        $userinfo = "";
                        $menu = BlockMenu(SCREEN_AUTH, 2);
                        $content = BlockRegSucceed($newlogin);
                        echo BlockMain($userinfo, $menu, $content);
                    }
                }
            }
        }
    }

    ?>
