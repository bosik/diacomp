<?php

    function slashes(&$el)
    {
        if (is_array($el))
            foreach($el as $k=>$v)
                slashes($el[$k]);
        else 
            $el = stripslashes($el); 
    }
	
    // ���� ������� ��������� ��������������� �������� �� ������
    // http://phpfaq.ru/slashes
    
    if (ini_get('magic_quotes_gpc'))
    {
        slashes($_GET);
        slashes($_POST);    
        slashes($_COOKIE);
    }
?>