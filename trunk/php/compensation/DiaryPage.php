<?php
	include_once 'common.php'; // GetLines()

	class CustomRecord
	{

		/**
		 * Получает значение, записанное в строке после времени
		 * @param type $s Строка
		 * @return type Значение
		 */
		public static function GetValue($s)
		{
			return substr($s, 7);
		}

		/**
		 * Преобразует время в формате чч:мм в число минут после полуночи
		 * @param type $s Время
		 * @return type Число минут
		 */
		public static function DecodeTimeMin($s)
		{
			//return substr($s, 1, 5);

			$hour = intval(substr($s, 1, 2));
			$min = intval(substr($s, 4, 2));
			return $hour * 60 + $min;
		}

		/**
		 * Преобразуем число минут после полуночи в строковое время
		 * @param type $time Число минут
		 * @return type  Время
		 */
		public static function EncodeTimeMin($time)
		{
			$hour = (int) ($time / 60);
			$min = $time % 60;
			return f00($hour) . "." . f00($min);
		}
	}

	class BloodRecord
	{
		public $type = "blood";
		public $time;
		public $value;
		public $finger;

		function __construct($time, $value, $finger = -1)
		{
			$this->time = $time;
			$this->value = $value;
			$this->finger = $finger;
		}

		public static function decode($s)
		{
			$time = CustomRecord::DecodeTimeMin($s);
			$s = CustomRecord::GetValue($s);
			$value = "";
			$finger = "";
			$sep = strpos($s, "|");
			if (!$sep)
			{
				$value = $s;
				$finger = "-1";
			}
			else
			{
				$value = substr($s, 0, $sep);
				$finger = substr($s, $sep + 1);
			}

			return new BloodRecord($time, $value, $finger);
		}
	}

	class InsRecord
	{
		public $type = "ins";
		public $time;
		public $value;

		function __construct($time, $value)
		{
			$this->time = $time;
			$this->value = $value;
		}

		public static function decode($s)
		{
			$time = CustomRecord::DecodeTimeMin($s);
			$value = CustomRecord::GetValue($s);
			return new InsRecord($time, $value);
		}
	}

	class FoodMassed
	{
		public $name;
		public $prots;
		public $fats;
		public $carbs;
		public $value;
		public $mass;

		function __construct($name, $prots, $fats, $carbs, $value, $mass)
		{
			$this->name = $name;
			$this->prots = $prots;
			$this->fats = $fats;
			$this->carbs = $carbs;
			$this->value = $value;
			$this->mass = $mass;
		}

		public function encode()
		{
			return "#" . $this->name . "[" . $this->prots . "|" . $this->fats . "|" . $this->carbs . "|" . $this->value . "]:" . $this->mass;
		}

		public static function decode($s)
		{
			$s = substr($s, 1);
			list($name, $prots, $fats, $carbs, $value, $mass) = preg_split("/[]:|\\[]+/", $s);
			return new FoodMassed($name, $prots, $fats, $carbs, $value, $mass);
		}
	}

	class MealRecord
	{
		public $type = "meal";
		public $time;
		public $short;
		public $content = array();

		public function Add($food)
		{
			$this->content[] = $food;
		}

		function __construct($time, $short)
		{
			$this->time = $time;
			$this->short = $short;
		}

		public static function decode($s)
		{
			$time = CustomRecord::DecodeTimeMin($s);
			$short = (strpos($s, "s") != false);
			return new MealRecord($time, $short);
		}
	}

	class NoteRecord
	{
		public $type = "note";
		public $time;
		public $text;

		function __construct($time, $text)
		{
			$this->time = $time;
			$this->text = $text;
		}

		public static function decode($s)
		{
			$time = CustomRecord::DecodeTimeMin($s);
			$text = CustomRecord::GetValue($s);
			return new NoteRecord($time, $text);
		}
	}

	class DiaryPage
	{
		public $date = "";
		public $timestamp = "";
		public $version = 1;
		public $content = array();

		public function Add($record)
		{
			$this->content[] = $record;
		}

		public function LoadFromPlain($text, $headed = true)
		{
			$this->content = array();
			if ($text == "")
				return;

			$prev_food = false;
			$lines = GetLines($text);
			$start;
			if ($headed)
			{
				list($this->date, $this->timestamp, $this->version) = preg_split("/[|]+/", $lines[0]);
				$this->date = substr($this->date, 4, 10);
				$start = 1;
			}
			else
			{
				$start = 0;
			}

			for ($i = $start; $i < count($lines); $i++)
			{
				switch ($lines{$i}{0})
				{
					case "*":
						{
							$prev_food = false;
							$this->Add(BloodRecord::decode($lines{$i}));
							break;
						}
					case "-":
						{
							$prev_food = false;
							$this->Add(InsRecord::decode($lines{$i}));
							break;
						}
					case " ":
						{
							$prev_food = MealRecord::decode($lines{$i});
							$this->Add($prev_food);
							break;
						}
					case "#":
						{
							if ($prev_food)
							{
								$food = FoodMassed::decode($lines{$i});
								$prev_food->Add($food);
							}
							break;
						}
					case "%":
						{
							$prev_food = false;
							$this->Add(NoteRecord::decode($lines{$i}));
							break;
						}
				}
			}
		}

		public function SaveToPlain($headed = true)
		{
			$result = "";
			
			if ($headed)
			{
				$result .= "=== " . $this->date . " ===|" . $this->timestamp . "|" . $this->version . "\n";
			}

			foreach ($this->content as &$item)
			{
				if ($item->type == "blood")
				{
					$result .= "*" . CustomRecord::EncodeTimeMin($item->time) . " " . $item->value . "|" . $item->finger . "\n";
				}
				else
				if ($item->type == "ins")
				{
					$result .= "-" . CustomRecord::EncodeTimeMin($item->time) . " " . $item->value . "\n";
				}
				else

				if ($item->type == "meal")
				{
					$result .= " " . CustomRecord::EncodeTimeMin($item->time) . ($item->short ? "s" : "") . "\n";
					foreach ($item->content as &$food)
					{
						$result .= "#" . $food->name . "[" . $food->prots . "|" . $food->fats . "|" . $food->carbs . "|" . $food->value . "]:" . $food->mass . "\n";
						//$food_cast = new FoodMassed($food->name, $food->prots, $food->fats, $food->carbs, $food->value, $food->mass);						
						//$result .= $food_cast->encode() + "\n";
					}
				}
				else
				if ($item->type == "note")
				{
					$result .= "%" . CustomRecord::EncodeTimeMin($item->time) . " " . $item->text . "\n";
				}
			}

			return $result;
		}

		public function SaveToJSON()
		{
			$this->Encode('windows-1251', 'utf-8');
			$result = json_encode($this);
			$this->Encode('utf-8', 'windows-1251');

			return $result;
		}

		public function Encode($from, $to)
		{
			foreach ($this->content as &$item)
			{
				if ($item->type == "meal")
				{
					foreach ($item->content as &$food)
					{
						$food->name = mb_convert_encoding($food->name, $to, $from);
					}
				}
				else
				if ($item->type == "note")
				{
					$item->text = mb_convert_encoding($item->text, $to, $from);
				}
			}
		}
	}

?>
