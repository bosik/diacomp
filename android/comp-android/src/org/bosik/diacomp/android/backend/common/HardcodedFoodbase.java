package org.bosik.diacomp.android.backend.common;

import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.utils.Utils;

public class HardcodedFoodbase
{
	private static void addFood(FoodItem food)
	{
		if (Storage.localFoodBase.findOne(food.getName()) == null)
		{
			Versioned<FoodItem> item = new Versioned<FoodItem>(food);
			item.setId(Utils.generateGuid());
			Storage.localFoodBase.add(item);
		}
	}

	private static FoodItem item(String name, double relProts, double relFats, double relCarbs, double relValue)
	{
		int tag = 0;
		boolean fromTable = true;
		return new FoodItem(name.trim(), relProts, relFats, relCarbs, relValue, tag, fromTable);
	}

	public static void restoreHardcodedBase()
	{
		addFood(item("iБлин со сгущенным молоком (Теремок)", 5.51, 10.96, 36.69, 267.43));
		addFood(item("Mirinda (Теремок)", 0, 0, 10.8, 44.0));
		addFood(item("Mirinda (Теремок)", 0, 0, 10.8, 44.0));
		addFood(item("Pepsi (Теремок)", 0, 0, 10.8, 44.0));
		addFood(item("Pepsi (Теремок)", 0, 0, 10.8, 44.0));
		addFood(item("Pepsi light (Теремок)", 0, 0, 0, 0.3));
		addFood(item("Pepsi light (Теремок)", 0, 0, 0, 0.3));
		addFood(item("Seven up (Теремок)", 0, 0, 10.8, 44.0));
		addFood(item("Seven up (Теремок)", 0, 0, 10.8, 44.0));
		addFood(item("Банан (Теремок)", 1.5, 0.5, 21, 96));
		addFood(item("Банановый десерт с вишней и миндалём (Теремок)", 1.57, 1.79, 26.87, 129.92));
		addFood(item("Банановый десерт с карамелью и миндалём (Теремок)", 2.03, 5.59, 30.77, 181.53));
		addFood(item("Банановый десерт с клубникой и миндалём (Теремок)", 2.16, 2.06, 28.48, 141.11));
		addFood(item("Банановый десерт с мёдом и миндалём (Теремок)", 1.9, 1.92, 37.8, 173.2));
		addFood(item("Банановый десерт с шоколадом и миндалём (Теремок)", 4.21, 11.66, 32.23, 250.76));
		addFood(item("Банановый десерт с яблоком и миндалём (Теремок)", 1.79, 1.79, 21.03, 107.47));
		addFood(item("Банановый десерт со сгущённым молоком и миндалём (Теремок)", 4.05, 3.5, 33.19, 180.55));
		addFood(item("Бекон  (Теремок)", 8, 65, 0, 617));
		addFood(item("Блин \"Алёша Попович\" с добавкой \"грибы\" (Теремок)", 10.75, 17.13, 13.53, 251.28));
		addFood(item("Блин \"Алёша Попович\" с добавкой \"зелень\" (Теремок)", 11.93, 21.3, 16.82, 306.7));
		addFood(item("Блин \"Алёша Попович\" с добавкой \"картофельное пюре\" (Теремок)", 9.74, 18.62, 13.84, 261.95));
		addFood(item("Блин \"Алёша Попович\" с добавкой \"сыр\" (Теремок)", 14.71, 22.72, 14.24, 320.35));
		addFood(item("Блин \"Алёша Попович\" с куриной грудкой-гриль под яблочным соусом (Теремок)", 11.52, 11.9,
				15.48, 215.1));
		addFood(item("Блин \"Греческий\" с брынзой, беконом и зеленью (Теремок)", 5.55, 27.02, 16.03, 329.59));
		addFood(item("Блин \"Две сосиски\" (Теремок)", 7.03, 15.03, 15.61, 225.79));
		addFood(item("Блин \"Две сосиски\" (Теремок)", 7.03, 15.03, 15.61, 225.79));
		addFood(item("Блин \"Змей Горыныч\" с горчицей (Теремок)", 6.77, 18.36, 21.45, 278.12));
		addFood(item("Блин \"Змей Горыныч\" с перцем чили (Теремок)", 5.86, 18.38, 16.42, 236.15));
		addFood(item("Блин \"Золотая рыбка\" с сёмгой под сырным соусом и зеленью (Теремок)", 8.41, 13.78, 18.98,
				233.55));
		addFood(item("Блин \"Золотая рыбка\" с сёмгой под сырным соусом и зеленью. (Теремок)", 8.41, 13.78, 18.98,
				233.55));
		addFood(item("Блин \"Илья Муромец\" с бужениной под красным соусом, с грибами,сыром и зеленью (Теремок)",
				10.18, 22.12, 11.35, 285.24));
		addFood(item("Блин \"Итальяно\" (Теремок)", 11.08, 15.13, 14.79, 239.69));
		addFood(item("Блин \"Лакомка\" со сливочным творожком и курагой (Теремок)", 8.07, 15.18, 18.96, 244.75));
		addFood(item("Блин \"Оливье\" (Теремок)", 4.06, 15.69, 18.78, 232.62));
		addFood(item("Блин \"Роял\" (Теремок)", 7.94, 15.46, 17.12, 239.37));
		addFood(item("Блин \"Три сыра\" (Теремок)", 9.38, 14.78, 15.97, 234.43));
		addFood(item("Блин \"Тропический\" с шоколадной начинкой и бананом (Теремок)", 4.15, 11.91, 30.36, 245.24));
		addFood(item("Блин \"Фермерский\" (Теремок)", 5.21, 12.33, 16.4, 197.35));
		addFood(item("Блин \"Цезарь\" (Теремок)", 10.54, 12.62, 14.26, 212.77));
		addFood(item("Блин \"Добрыня Никитич\" с рубленым мясом/Большой мясной (Теремок)", 7.18, 15.91, 17.62, 224.82));
		addFood(item("Блин E-mail с грибами (Теремок)", 4.51, 9.37, 18.15, 174.95));
		addFood(item("Блин E-mail с грибами и  куриной грудкой (Теремок)", 11.06, 7.86, 13.45, 168.78));
		addFood(item("Блин E-mail с грибами, сыром и зеленью (Теремок)", 8.16, 13.08, 15.14, 210.93));
		addFood(item("Блин с бужениной под красным соусом (Теремок)", 9.29, 26.98, 16.49, 345.97));
		addFood(item("Блин с ветчиной (Теремок)", 4.74, 11.44, 29.97, 241.83));
		addFood(item("Блин с грибами и сыром (Теремок)", 8.19, 13.12, 14.94, 210.64));
		addFood(item("Блин с двойной красной икрой (Теремок)", 15.99, 12.14, 16.24, 238.12));
		addFood(item("Блин с капустой и яйцом (Теремок)", 4.3, 11.3, 18.39, 192.43));
		addFood(item("Блин с карамелью (Теремок)", 3.51, 11.64, 34.28, 255.88));
		addFood(item("Блин с красной икрой (Теремок)", 11.91, 12.31, 20.19, 239.18));
		addFood(item("Блин с маслом (Теремок)", 4.66, 12.63, 27.21, 241.08));
		addFood(item("Блин с мёдом (Теремок)", 3.69, 10.44, 40.75, 271.74));
		addFood(item("Блин с рубленым мясом и картофельным пюре  (Теремок)", 5.74, 13.18, 15.98, 205.47));
		addFood(item("Блин с сёмгой (Теремок)", 8.76, 11.83, 20.24, 222.43));
		addFood(item("Блин с сёмгой (Теремок)", 8.76, 11.83, 20.24, 222.43));
		addFood(item("Блин с сыром (Теремок)", 9.95, 17.24, 20.23, 275.83));
		addFood(item("Блин с сыром (Теремок)", 9.95, 17.24, 20.23, 275.83));
		addFood(item("Блин с шоколадной начинкой (Теремок)", 5.75, 19.03, 35.99, 338.22));
		addFood(item("Блин с яблоком (Теремок)", 3.46, 8.83, 25.61, 195.69));
		addFood(item("Блин СВ Двойной с сыром и ветчиной (Теремок)", 8.94, 15.35, 23.86, 269.33));
		addFood(item("Блин СВ Двойной с сыром и ветчиной + грибы (Теремок)", 7.71, 12.35, 18.41, 215.65));
		addFood(item("Блин со сливочной карамелью и яблоком (Теремок)", 2.93, 9.87, 31.61, 226.96));
		addFood(item("Блин со сметаной (Теремок)", 3.92, 16.16, 19.06, 237.37));
		addFood(item("Блин со сметаной (Теремок)", 3.92, 16.16, 19.06, 237.37));
		addFood(item("Блинчик с вишней (Теремок)", 3.74, 10.75, 37.73, 262.61));
		addFood(item("Блинчик с грибами (Теремок)", 4.46, 12.18, 18.34, 200.82));
		addFood(item("Блинчик с клубникой (Теремок)", 3.72, 11.84, 34.73, 260.3));
		addFood(item("Блинчик с сосиской и горчицей (Теремок)", 7.03, 17.75, 16.28, 253.07));
		addFood(item("Блинчик с сосиской и кетчупом (Теремок)", 5.81, 13.97, 17.83, 220.2));
		addFood(item("Блинчик с сосиской и красным соусом (Теремок)", 6.43, 18.28, 15.58, 252.56));
		addFood(item("Блинчик с сосиской и сырным соусом (Теремок)", 6.81, 20.42, 15.27, 272.16));
		addFood(item("Блинчик с сосиской и яблочным соусом (Теремок)", 5.54, 16.58, 17.21, 240.24));
		addFood(item("Блинчик с сыром и беконом (Теремок)", 8.62, 30.82, 15.33, 373.17));
		addFood(item("Блинчик с сыром и ветчиной (Теремок)", 8.21, 17.47, 23.8, 285.28));
		addFood(item("Борщ с беконом (Теремок)", 1.33, 9.84, 3.62, 108.31));
		addFood(item("Борщ с рубленым мясом  (Теремок)", 2.53, 6.83, 5.31, 92.82));
		addFood(item("Борщ  постный (Теремок)", 0.54, 2.79, 4, 43.27));
		addFood(item("Брынза  (Теремок)", 6.11, 34.66, 2.04, 344.56));
		addFood(item("Буженина (Теремок)", 17.63, 30.67, 0, 346.55));
		addFood(item("Ветчина  (Теремок)", 10.4, 13.8, 0, 165.75));
		addFood(item("Винегрет постный (Теремок)", 1.17, 9.12, 7, 114.78));
		addFood(item("Вишня (Теремок)", 0, 0, 50, 200));
		addFood(item("Гороховый суп постный (Теремок)", 3.29, 1.25, 7.17, 53.07));
		addFood(item("Гороховый суп с копченостями (Теремок)", 3.66, 6.24, 6.61, 97.2));
		addFood(item("Горчица (Теремок)", 9.6, 4, 11, 118));
		addFood(item("Гречка \"Две сосиски\" (Теремок)", 6.66, 9.73, 13.56, 168.46));
		addFood(item("Гречка постная (Теремок)", 4.01, 4.34, 18.71, 129.97));
		addFood(item("Гречка с брынзой, беконом и зеленью (Теремок)", 5.39, 19.79, 13.85, 255.04));
		addFood(item("Гречка с бужениной и красным соусом (Теремок)", 8.7, 19.75, 14.36, 262.24));
		addFood(item("Гречка с ветчиной и сыром (Теремок)", 8.26, 9.64, 20.44, 201.52));
		addFood(item("Гречка с грибами (Теремок)", 4.49, 4.34, 15.48, 118.93));
		addFood(item("Гречка с капустой и яйцом (Теремок)", 4.32, 6.23, 15.82, 136.58));
		addFood(item("Гречка с котлетой (Теремок)", 7.92, 8.3, 18.46, 180.2));
		addFood(item("Гречка с куриной грудкой-гриль и яблочным соусом (Теремок)", 10.35, 6.16, 16.94, 155.56));
		addFood(item("Гречка с рубленым мясом (Теремок)", 6.87, 8.45, 15.44, 165.35));
		addFood(item("Гречка с семгой (Теремок)", 8.01, 5.86, 16.93, 152.51));
		addFood(item("Гречка с тефтелями (Теремок)", 6.58, 8.39, 11.61, 148.27));
		addFood(item("Гречка с треской (Теремок)", 7.91, 6.58, 14.92, 150.51));
		addFood(item("Грибной суп с чипами (Теремок)", 1.95, 3.4, 2.6, 48.81));
		addFood(item("Грибы (Теремок)", 4.28, 3.96, 3.15, 65.36));
		addFood(item("Десерт \" Гурьевский\" с чипами (Теремок)", 3.28, 10.25, 11.13, 149.96));
		addFood(item("Десерт \"Гурьевский\" с вишней и миндальным орехом (Теремок)", 2.49, 5.47, 22.32, 160.45));
		addFood(item("Десерт \"Гурьевский\" с карамелью, яблоком и миндальным орехом (Теремок)", 2.78, 9.33, 20, 175.03));
		addFood(item("Десерт \"Гурьевский\" с клубникой и миндальным орехом (Теремок)", 3.39, 9.89, 16.62, 169.03));
		addFood(item("Десерт \"Гурьевский\" с мёдом и миндальным орехом (Теремок)", 3.19, 9.51, 21.93, 186.04));
		addFood(item("Десерт \"Гурьевский\" с шоколадом и миндальным орехом (Теремок)", 2.49, 5.47, 22.32, 230.57));
		addFood(item("Десерт \"Гурьевский\" со сгущенным молоком и миндальным орехом (Теремок)", 4.38, 9.9, 20.71,
				189.49));
		addFood(item("Зелень (Теремок)", 3.7, 0.4, 8, 50.4));
		addFood(item("Иван-чай (Теремок)", 0, 0, 6.5, 26));
		addFood(item("Капуста с яйцом  (Теремок)", 3.79, 9.4, 5.62, 122.21));
		addFood(item("Карамель (Теремок)", 1.6, 10, 46, 296));
		addFood(item("Картофель постный (Теремок)", 9.7, 0.75, 18.9, 174.08));
		addFood(item("Картофельное пюре  (Теремок)", 1.75, 11.75, 11.33, 158.1));
		addFood(item("Квас \"Теремок\" (Теремок)", 0, 0, 7, 28));
		addFood(item("Квас \"Теремок\" (Теремок)", 0, 0, 7, 28));
		addFood(item("Кетчуп (Теремок)", 0.9, 0, 23.4, 97));
		addFood(item("Клубника  (Теремок)", 0, 0, 56, 216));
		addFood(item("Котлета (Теремок)", 10, 33, 3, 349));
		addFood(item("Кофе \"Американо\" (Теремок)", 0, 0, 4.2, 16.8));
		addFood(item("Кофе \"Двойной эспрессо\" (Теремок)", 0, 0, 4.2, 16.8));
		addFood(item("Кофе \"Капучино\" (Теремок)", 1.5, 1.6, 5.5, 42.5));
		addFood(item("Кофе \"Латте\" (Теремок)", 3, 3, 18, 112));
		addFood(item("Кофе \"Эспрессо\" (Теремок)", 0, 0, 4.2, 16.8));
		addFood(item("Куриная грудка-гриль (Теремок)", 29.82, 3.5, 0, 151.12));
		addFood(item("Лук-фри  (Теремок)", 4.5, 13.5, 27.4, 249.1));
		addFood(item("Майонез (Теремок)", 0.8, 50, 3.6, 467.6));
		addFood(item("Мёд (Теремок)", 0, 0, 71, 315));
		addFood(item("Мёд хмельной (Теремок)", 0, 0, 10.5, 85));
		addFood(item("Мёд хмельной (Теремок)", 0, 0, 10.5, 85));
		addFood(item("Молочный коктейль \"Классический\" (Теремок)", 2.99, 5.87, 10.63, 107.34));
		addFood(item("Молочный коктейль \"Клюквенный\" (Теремок)", 3.95, 14.14, 10.03, 1.34));
		addFood(item("Молочный коктейль с бананом (Теремок)", 2.66, 4.57, 12.96, 103.65));
		addFood(item("Молочный коктейль с карамелью (Теремок)", 2.76, 6.57, 16.6, 136.53));
		addFood(item("Молочный коктейль с клубникой (Теремок)", 2.88, 5.4, 14.26, 117.19));
		addFood(item("Молочный коктейль с шоколадом (Теремок)", 3.65, 9, 16.45, 161.43));
		addFood(item("Морс клюквенный (Теремок)", 0.06, 0.03, 10.49, 42.2));
		addFood(item("Морс клюквенный (Теремок)", 0.06, 0.03, 10.49, 42.2));
		addFood(item("Огурцы  (Теремок)", 0.34, 0.07, 1.09, 6.35));
		addFood(item("Окрошка домашняя на квасе  (Теремок)", 2.14, 4.06, 9.73, 83.96));
		addFood(item("Орех миндальный  (Теремок)", 18.6, 53.7, 13, 380.2));
		addFood(item("Паштет (Теремок)", 13.45, 18.66, 4.02, 237.84));
		addFood(item("Пельмени \"Сибирский деликатес\" (Теремок)", 7.7, 25.5, 27.5, 371));
		addFood(item("Пиво \"Жигули\" (Теремок)", 0, 0, 3.8, 43));
		addFood(item("Рубленое мясо  (Теремок)", 11.17, 15.89, 4.61, 206.11));
		addFood(item("Салат \"Афинский\" (Теремок)", 4.65, 16.59, 2.87, 179.44));
		addFood(item("Салат \"Оливье\" (Теремок)", 4.46, 13.27, 5.4, 158.94));
		addFood(item("Салат \"Русский Цезарь\" (Теремок)", 13.19, 25.49, 5.33, 303.47));
		addFood(item("Салат Винегрет с семгой (Теремок)", 5.13, 11.01, 6.72, 146.44));
		addFood(item("Сбитень (Теремок)", 0, 0, 9.47, 37.88));
		addFood(item("Сгущенное молоко (Теремок)", 7.2, 8.5, 43.5, 315));
		addFood(item("Сёмга (Теремок)", 20.2, 11.04, 0, 179.8));
		addFood(item("Сметана  (Теремок)", 4.8, 46, 6.4, 248));
		addFood(item("Соус белый (Теремок)", 5.66, 36.64, 3.1, 364.8));
		addFood(item("Соус Карри порционный (Теремок)", 1.1, 1, 20, 93));
		addFood(item("Соус кисло-сладкий порционный (Теремок)", 0.2, 1.1, 23, 103));
		addFood(item("Соус красный с хреном  (Теремок)", 2.71, 16.74, 7.96, 193.39));
		addFood(item("Соус сырный порционный (Теремок)", 1.5, 55, 2.5, 511));
		addFood(item("Соус чесночный порционный (Теремок)", 1.1, 45, 2.5, 419));
		addFood(item("Соус яблочный  (Теремок)", 1.56, 19.28, 6.93, 207.46));
		addFood(item("Суп куриный \"Золотой\" (Теремок)", 5.27, 3.36, 2.42, 61.06));
		addFood(item("Сыр (Теремок)", 23.6, 30.2, 0.1, 370));
		addFood(item("Сырники с вишней (2 шт.) (Теремок)", 9.67, 4.74, 30.91, 205.01));
		addFood(item("Сырники с вишней (3 шт.) (Теремок)", 10.72, 5.26, 29.92, 209.9));
		addFood(item("Сырники с карамелью (2 шт.) (Теремок)", 9.24, 7.93, 34.07, 244.63));
		addFood(item("Сырники с карамелью (3 шт.) (Теремок)", 10.31, 7.64, 32.41, 239.62));
		addFood(item("Сырники с клубникой (2 шт.) (Теремок)", 11.28, 5.37, 32.89, 225.09));
		addFood(item("Сырники с клубникой (3 шт.) (Теремок)", 11.97, 5.76, 31.25, 224.71));
		addFood(item("Сырники с мёдом светлым /тёмным (2 шт.) (Теремок)", 10.48, 5.04, 40.41, 248.92));
		addFood(item("Сырники с мёдом светлым /тёмным (3 шт.) (Теремок)", 11.35, 5.5, 36.79, 242.06));
		addFood(item("Сырники с шоколадной начинкой (2 шт.) (Теремок)", 12.08, 13.74, 35.79, 315.18));
		addFood(item("Сырники с шоколадной начинкой (3 шт.) (Теремок)", 11.07, 7.43, 31.23, 236.07));
		addFood(item("Сырники с яблоком (2 шт.) (Теремок)", 9.88, 4.74, 25.58, 184.54));
		addFood(item("Сырники с яблоком (3 шт.) (Теремок)", 10.87, 5.26, 25.99, 194.77));
		addFood(item("Сырники со сгущенным молоком (2 шт.) (Теремок)", 11.5, 6.14, 36.48, 247.24));
		addFood(item("Сырники со сгущенным молоком (3 шт.) (Теремок)", 12.05, 6.29, 34.14, 241.37));
		addFood(item("Сырники со сметаной (2 шт.) (Теремок)", 9.97, 12.14, 19.18, 225.91));
		addFood(item("Сырники со сметаной (3 шт.) (Теремок)", 10.9, 10.79, 21.17, 225.37));
		addFood(item("Сырный крем-суп (Теремок)", 2.79, 12.26, 2.97, 133.38));
		addFood(item("Тефтели, 1шт. (Теремок)", 8.34, 20.26, 4.59, 443.04));
		addFood(item("Уха из форели по-фински (Теремок)", 4.63, 4.57, 2.33, 68.97));
		addFood(item("Чай Nik Tea в ассорт. (Теремок)", 0, 0, 6.5, 26));
		addFood(item("Чай Альтхаус в ассорт. (Теремок)", 0, 0, 6.5, 26));
		addFood(item("Шоколадная начинка (Теремок)", 1.6, 10, 46, 296));
		addFood(item("Яблоко (Теремок)", 0, 0, 18.5, 74));
	}
}