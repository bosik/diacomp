// компоненты
var calendar = document.getElementById("calendar");
var modspan = document.getElementById("modspan");
var versionspan = document.getElementById("versionspan");
var diary = document.getElementById("diary_page");
var infoblock = document.getElementById("diary_info");

// константы
var fingers = ["БЛ", "1Л", "2Л", "3Л", "4Л", "4П", "3П", "2П", "1П", "БП"];
var finger_hints = [
"Левая, большой",
"Левая, указательный",
"Левая, средний",
"Левая, безымянный",
"Левая, мизинец",
"Правая, мизинец",
"Правая, безымянный",
"Правая, средний",
"Правая, указательный",
"Правая, большой"
];
var PERIOD = 1440;
var BLOOD_ACTUAL_PERIOD = 60;
var INS_ACTUAL_PERIOD = 90;
var TARGET_BS = 5.0; // TODO: load from user properties

// данные
var cur_date = new Date(Date.parse(document.getElementById("origin_date").value));
var page = [];
var koofs = [];
var foodbase = [];

var fdBase = [];
var selID;
var currentID;

/* ================== STARTUP ACTIONS ================== */



// TODO: download dish base
downloadKoofs();
downloadFoodbase();

refreshCurrentPage();

/* ================== DIARY NAVIGATION ================== */

function refreshCurrentPage()
{
	downloadPage();
	showInfoBox(-1);
	calendar.value = formatDate(cur_date);
}

function shiftDate(days)
{
	var newDate = new Date();
	newDate.setTime(cur_date.getTime() + 86400000 * days);
	cur_date = newDate;
}

function prevDay()
{
	shiftDate(-1);
	pushHistory("index.php?date=" + formatDate(cur_date));
	refreshCurrentPage();
}

function nextDay()
{
	shiftDate(+1);
	pushHistory("index.php?date=" + formatDate(cur_date));
	refreshCurrentPage();
}

function openPage()
{
// TODO: implement
}

/* ================== INTERNET ================== */

function downloadPage()
{
	//diary.innerHTML = "Загрузка..." ;
	var url = "console.php?diary:download&format=json&dates=" + formatDate(cur_date);

	var onSuccess = function(data)
	{
		// debug only
		//diary.innerHTML = xmlhttp.responseText;
		diary.innerHTML = "Loaded ok, parsing...";

		page = JSON.parse(data);

		// debug only
		diary.innerHTML = "Parsed ok, sorting...";

		page.content.sort(sortFunction);

		// debug only
		diary.innerHTML = "Sorted ok, rendering...";

		//document.getElementById("debug").innerHTML = ObjToSource(page);
		showPage();
	};

	var onFailure = function ()
	{
	//diary.innerHTML = "Не удалось загрузить страницу";
	}

	download(url, onSuccess, onFailure);
}

function downloadKoofs()
{
	var url = "console.php?koofs:download";

	var onSuccess = function(data)
	{
		if (data != "")
		{
			//alert("'" + data + "'");
			koofs = JSON.parse(data);
		//alert(ObjToSource(koofs));
		}
		else
		{
			koofs = [];
		}
	};

	var onFailure = function ()
	{
		koofs = [];
	}

	download(url, onSuccess, onFailure);
}

function downloadFoodbase()
{
	// загрузка

	var url = "console.php?foodbase:download";

	var onSuccess = function(data)
	{
		if (data != "")
		{
			data = getAfter(data, "\n");

			//	data = '<foods version="8">'+
			//		'<food name="Абрикос" prots="0.9" fats="0.1" carbs="9" val="41" table="True"/>'+
			//		'<food name="Банан" prots="1.9" fats="2.1" carbs="30" val="60" table="True"/>'+
			//		'</foods>';


			//data = data.replace(/&quot;/g, '\\"');
			data = data.replace(/&quot;/g, '');
			//alert("data='" + data + "'");

			var dom = parseXml(data);
			var json = xml2json(dom, "");

			json = json.replace(/undefined/g, "").replace(/@/g, "");
			//alert("json='" + json + "'");

			foodbase = JSON.parse(json).foods;

			//alert("json[2]='" + ObjToSource(foodbase) + "'");
			//alert("version = " + foodbase.version);

			//========================================

			// подготовка

			for (var i = 0; i < foodbase.food.length; i++)
			{
				foodbase.food[i].id = i;
			}

			var sortFunction = function (a, b)
			{
				if (a.name < b.name) return -1; // Или любое число, меньшее нуля
				if (a.name > b.name) return +1; // Или любое число, большее нуля
				return 0;
			}

			foodbase.food.sort(sortFunction);
			//alert("foodbase loaded: " + ObjToSource(foodbase));

			// preparing autocomplete

			for (i = 0; i < foodbase.food.length; i++)
			{
				var item = [];
				item.value = foodbase.food[i].name;
				item.type = "food";
				item.id = foodbase.food[i].id;
				fdBase.push(item);
			}
		}
		else
		{
			foodbase = [];
			fdBase = [];
		}
	};

	var onFailure = function ()
	{
		foodbase = [];
	}

	download(url, onSuccess, onFailure);
}

function uploadPage()
{
	var url = "console.php";
	var request = 'diary:upload=&format=json&pages=' + encodeURIComponent(ObjToSource(page));

	var onSuccess = function (resp)
	{
		//document.getElementById("summa").innerHTML = xmlhttp.responseText; // Выводим ответ сервера
		if (resp == "DONE")
		{
		//alert("Saved OK");
		}
		else
		{
			alert("Failed to save with message '" + resp + "'");
		}
	}

	var onFailure = function ()
	{
		alert("Failed to save page");
	}

	upload(url, request, onSuccess, onFailure);
}

/* ================== UTILS ================== */

function sortFunction(a, b)
{
	return a.time - b.time;
}

/* ================== KOOF UTILS ================== */

function getR(left, right, time)
{
	if (left > right) right += PERIOD;
	if (time < left) time += PERIOD;
	if (time > right) time -= PERIOD;

	return (time - left) / (right - left);
}

function interpolateBi(v1, v2, time)
{
	var r = getR(v1.time, v2.time, time);

	var _k = (v1.k * (1 - r) + v2.k * r).toFixed(4);
	var _q = (v1.q * (1 - r) + v2.q * r).toFixed(2);
	var _p = (v1.p * (1 - r) + v2.p * r).toFixed(2);

	return {
		k: _k,
		q: _q,
		p: _p
	};
}

function interpolate(koofList, time)
{
	if ((koofList == null) || (koofList.length == 0)) return null;

	//alert(ObjToSource(koofList));

	if ((time < koofList[0].time) || (time >= koofList[koofList.length-1].time))
		return interpolateBi(koofList[koofList.length-1], koofList[0], time);

	for (i = 0; i < koofList.length-1; i++)
		if ((time >= koofList[i].time) && (time < koofList[i+1].time))
			return interpolateBi(koofList[i], koofList[i + 1], time);

	return null;
}

/* ================== PRINTERS (PAGE) ================== */

function codeBlood(blood, id)
{
	var value = strToFloat(blood.value).toFixed(1);
	var finger = fingers[blood.finger];
	var finger_hint = finger_hints[blood.finger];

	if (null == finger) finger = "";
	if (null == finger_hint) finger_hint = "";

	return '' +
	'				<div id="diaryRec_'+id+'" class="rec blood" onclick="onRecordClick(this.id)">\n'+
	'					<div class="time hoverable" id="time_' + id + '" onclick="onTimeClick(this.id)">' + formatTime(blood.time) + "</div>\n" +
	'					<div class="item">' +
	"						<table cellpadding=\"0\">\n" +
	"							<tr>\n" +
	'								<td class="col_item"><span id="item_' + id + '" class="hoverable" onclick="onBloodClick(this.id)">' + value + ' ммоль/л</span></td>\n' +
	"								<td class=\"col_info\"><div id=\"item_" + id + "\" title=\"" + finger_hint + "\">" + finger + "</div></td>\n" +
	"								<td class=\"col_delete\"><div id=\"item_" + id + "\" onclick=\"onDeleteClick(this.id)\" title=\"Удалить\">X</div></td>\n" +
	"							</tr>\n" +
	"						</table>\n" +
	'					</div>' +
	'				</div>';
}

function codeIns(ins, id)
{
	return '' +
	'				<div id="diaryRec_' + id + '" class="rec ins" onclick="onRecordClick(this.id)">\n'+
	'					<div class="time hoverable" id="time_' + id + '" onclick="onTimeClick(this.id)">' + formatTime(ins.time) + "</div>\n" +
	'					<div class="item">' +
	"						<table cellpadding=\"0\">\n" +
	"							<tr>\n" +
	'								<td class="col_item"><span id="item_' + id + '" class="hoverable" onclick="onInsClick(this.id)">' + ins.value + ' ед</span></td>\n' +
	"								<td class=\"col_info\"></td>\n" +
	"								<td class=\"col_delete\"><div id=\"item_" + id + "\" onclick=\"onDeleteClick(this.id)\" title=\"Удалить\">X</div></td>\n" +
	"							</tr>\n" +
	"						</table>\n" +
	'					</div>' +
	'				</div>';
}

function codeMeal(meal, id)
{
	var t = (meal.short ? "short_meal" : "meal");
	var code = '';
	code +=
	'				<div id="diaryRec_' + id + '" class="rec ' + t + '" onclick="onRecordClick(this.id)">\n';

	code +=
	'					<div class="time hoverable" id="time_' + id + '" onclick="onTimeClick(this.id)">' + formatTime(meal.time) + "</div>\n" +
	'					<div class="item">\n' +
	"						<table cellpadding=\"0\">\n";
	for (var i = 0; i < meal.content.length; i++)
	{
		code +=
		"							<tr class=\"food\">\n" +
		"								<td class=\"col_item\"><span id=\"food_" + id + "_" + i + "\">" + meal.content[i].name + "</span></td>\n" +
		'								<td class="col_info"><div id="food_' + id + '_' + i + '" class="hoverable" onclick="onFoodMassClick(this.id)" title="Изменить массу">' + meal.content[i].mass + '</div></td>\n' +
		"								<td class=\"col_delete\"><div id=\"food_" + id + "_" + i + "\" onclick=\"onRemoveFoodClick(this.id)\" title=\"Удалить\">X</div></td>\n" +
		"							</tr>\n";


	//code += codeFood(meal.content[i], id + "_" + i) + '<br/>\n';
	}

	code +=
	"							<tr class=\"food\">\n" +
	"								<td class=\"col_item\">\n"+
	"									<div class=\"wrapper_table\">\n"+
	"										<span class=\"wrapper_cell\">\n"+
	'											<input id="mealcombo_' + id + '" class="meal_input full_width bold ' + t + '" placeholder="Введите название..."/>\n'+
	"										</span>\n"+
	"									</div>\n"+
	'								</td>\n' +
	'								<td class="col_info">\n'+
	'									<div class="wrapper_table">\n'+
	'										<span class="wrapper_cell">\n'+
	'											<input id="mealmass_' + id + '" class="meal_input full_width bold ' + t + '" type="number" placeholder="..." title="Масса" onkeypress="runScript(event,'+id+')"/>\n'+
	'										</span>\n'+
	'									</div>\n'+
	'								</td>\n' +
	"								<td class=\"col_delete\"><button onclick=\"runScript(event)\" title=\"Добавить\">+</button></td>\n" +
	"							</tr>\n";

	code +=
	"						</table>\n" +
	'					</div>\n' +
	'				</div>\n';
	return code;
}

function codeNote(note, id)
{
	return ''+
	'				<div id="diaryRec_' + id + '" class="rec note" onclick="onRecordClick(this.id)">\n'+
	'					<div class="time hoverable" id="time_' + id + '" onclick="onTimeClick(this.id)">' + formatTime(note.time) + "</div>\n" +
	'					<div class="item">' +
	"						<table cellpadding=\"0\">\n" +
	"							<tr>\n" +
	'								<td class="col_item"><span id="item_' + id + '" class="hoverable" onclick="onNoteClick(this.id)">' + note.text + '</span></td>\n' +
	"								<td class=\"col_info\"></td>\n" +
	"								<td class=\"col_delete\"><div id=\"item_" + id + "\" onclick=\"onDeleteClick(this.id)\" title=\"Удалить\">X</div></td>\n" +
	"							</tr>\n" +
	"						</table>\n" +

	'					</div>' +
	'				</div>';
}

/*function updateMeal()
			{
				var carbs = 0;
				var id = 0;
				for (var i = 0; i < meal.content.length; i++)
				{
					code += codeFood(meal.content[i], id++) + "<br>";
					carbs += meal.content[i].carbs * meal.content[i].mass / 100;
				}
				document.getElementById("meal").innerHTML = code;
				document.getElementById("meal_carbs").innerHTML = carbs.toFixed(1);
				document.getElementById("meal_dose").innerHTML = (carbs * 0.15).toFixed(1);
			}*/

function codePage(page)
{
	var code = '';//'			<div class="diary">';
	for (var i = 0; i < page.length; i++)
	{
		if (page[i].type == "meal") code += codeMeal(page[i], i);
		else
		if (page[i].type == "blood") code += codeBlood(page[i], i);
		else
		if (page[i].type == "ins") code += codeIns(page[i], i);
		else
		if (page[i].type == "note") code += codeNote(page[i], i);
	}
	//code += '			</div>';
	/*code +=
'	<div class="ui-widget">\n' +
'		<span id="meal"/>\n' +
'		<br/>\n' +
'		<label for="fdAutocomplete">Выберите продукт или блюдо:</label>\n' +
'		<div>\n' +
'			<div id="block_ok">\n' +
'				<button title="Добавить (Enter)">+</button>\n' +
'			</div>\n' +
'			<div id="block_mass">\n' +
'				<input class="myinput" id="mass"\n' +
'					onkeypress="return runScript(event)" type="number"/>\n' +
'			</div>\n' +
'			<div id="block_fd">\n' +
'				<input class="myinput" id="fdAutocomplete"/>\n' +
'			</div>\n' +
'		</div>\n' +
'	</div>';*/
	return code;
}

function createHandlers()
{
	console.log("createHandlers()");
	for (var i = 0; i < page.content.length; i++)
	{
		if (page.content[i].type == "meal")
		{
			console.log("createHandlers(): id=" + i);

			var id = i;
			// вешаем обработчики
			$(function() {
				$("#mealcombo_" + id).autocomplete({
					autoFocus: true,
					source: fdBase,
					delay: 0
				});
			});
			$("#mealcombo_" + id).on("autocompleteselect", function(event, ui)
			{
				selID = ui.item.id;
				//alert(fdBase[ui.item.id].value);
				//console.log("selected: event.id=" + event + ", selID=" + selID);
				console.log(ObjToSource(ui));


				var t = 'mealmass_' + currentID;
				var mc = document.getElementById(t);
				console.log("searching for component '" + t + "': " + mc);

				mc.focus();
			} );
		}
	}
}

function showPage()
{
	//alert("showPage(): page.length=" + page.content.length);

	if (page.content.length > 0)
	{
		diary.className = "diary_page_full";
		diary.innerHTML = codePage(page.content);
		var stamp = new Date(Date.parse(page.timestamp + " UTC"));
		modspan.innerHTML = formatTimestamp(stamp, true).replace(" ", "<br/>");
		versionspan.innerHTML = "";//"#" + page.version;
		createHandlers();
	}
	else
	{
		diary.className = "diary_page_empty";
		diary.innerHTML = "Страница пуста";
		modspan.innerHTML = "";
		versionspan.innerHTML = "";
	}
}

/* ================== PRINTERS (INFO PANEL) ================== */

function codeInfoDefault()
{
	return '' +
	"						Добавить новую запись:<br/><br/>\n" +
	"						<div>\n" +
	"							<div onclick=\"newBloodEditor()\" class=\"button_new_rec button_new_blood full_width\" title=\"Добавить замер СК\">Замер СК</div>\n" +
	"							<div onclick=\"newInsEditor()\" class=\"button_new_rec button_new_ins full_width\" title=\"Добавить инъекцию\">Инъекция</div>\n" +
	"							<div onclick=\"newMealEditor()\" class=\"button_new_rec button_new_meal full_width\" title=\"Добавить приём пищи\">Приём пищи</div>\n" +
	"							<div onclick=\"newNoteEditor()\" class=\"button_new_rec button_new_note full_width\" title=\"Добавить заметку\">Заметка</div>\n" +
	"						</div><hr>\n" +
	"						Для получения более подробной информации выберите запись на странице.\n";
}

function codeInfoBlood(rec)
{
	return '' +
	"						Вы выбрали замер СК.<br/><br/>\n<hr>" +
	"						Добавить новую запись:<br/><br/>\n" +
	"						<div>\n" +
	"							<button onclick=\"newBloodEditor()\" class=\"button_new_rec button_new_blood\" title=\"Добавить замер СК\"></button>\n" +
	"							<button onclick=\"newInsEditor()\" class=\"button_new_rec button_new_ins\" title=\"Добавить инъекцию\"></button>\n" +
	"							<button onclick=\"newNoteEditor()\" class=\"button_new_rec button_new_note\" title=\"Добавить заметку\"></button>\n" +
	"						</div>\n";
}

function codeInfoIns(rec)
{
	return '' +
	"						Вы выбрали инъекцию.<br/><br/>\n<hr>" +
	"						Добавить новую запись:<br/><br/>\n" +
	"						<div>\n" +
	"							<button onclick=\"newBloodEditor()\" class=\"button_new_rec button_new_blood\" title=\"Добавить замер СК\"></button>\n" +
	"							<button onclick=\"newInsEditor()\" class=\"button_new_rec button_new_ins\" title=\"Добавить инъекцию\"></button>\n" +
	"							<button onclick=\"newNoteEditor()\" class=\"button_new_rec button_new_note\" title=\"Добавить заметку\"></button>\n" +
	"						</div>\n";
}

function codeInfoMeal(meal)
{
	var prots = 0;
	var fats = 0;
	var carbs = 0;
	var val = 0;
	for (var i = 0; i < meal.content.length; i++)
	{

		prots += strToFloat(meal.content[i].prots) * strToFloat(meal.content[i].mass) / 100;
		fats += strToFloat(meal.content[i].fats) * strToFloat(meal.content[i].mass) / 100;
		carbs += strToFloat(meal.content[i].carbs) * strToFloat(meal.content[i].mass) / 100;
		val += strToFloat(meal.content[i].value) * strToFloat(meal.content[i].mass) / 100;
	//alert("New item: " + meal.content[i].carbs + " * " + meal.content[i].mass + "; summ is " + carbs);
	}

	var w = interpolate(koofs, meal.time);
	var blood = DiaryPage_findBlood(meal.time, BLOOD_ACTUAL_PERIOD);
	var BsTarget = TARGET_BS;
	var BsInput = (blood == null ? BsTarget : strToFloat(blood.value));
	var dose = null;
	var ins = DiaryPage_findIns(meal.time, INS_ACTUAL_PERIOD);
	var injectedDose = (ins == null ? null : strToFloat(ins.value).toFixed(1));
	var dk = null;

	//alert("injectedDose:" + injectedDose);

	if (w != null)
	{
		dose = ((BsInput - BsTarget + w.k * carbs + w.p * prots) / w.q).toFixed(1);
		if (injectedDose != null)
		{
			dk = ((BsTarget - BsInput - w.p*prots + w.q * injectedDose) / w.k - carbs).toFixed(0);
			if (dk > 0) dk = "+" + dk;
		}
	}

	if (injectedDose == null) injectedDose = "?";
	if (dose == null) dose = "?";
	if (dk == null) dk = "?";

	return '' +
	'<table cellpadding="1" style="text-align: left">\n'+
	'							<tr>\n'+
	'								<th class="full_width">Белки, г</th>\n'+
	'								<td class="right">'+prots.toFixed(0)+'</td>\n'+
	'							</tr>\n'+
	'							<tr>\n'+
	'								<th class="full_width">Жиры, г</th>\n'+
	'								<td class="right">'+fats.toFixed(0)+'</td>\n'+
	'							</tr>\n'+
	'							<tr>\n'+
	'								<th class="full_width">Углеводы, г</th>\n'+
	'								<td class="right">'+carbs.toFixed(0)+'</td>\n'+
	'							</tr>\n'+
	'							<tr>\n'+
	'								<th class="full_width">Ценность, ккал</th>\n'+
	'								<td class="right">'+val.toFixed(0)+'</td>\n'+
	'							</tr>\n'+
	'						</table>\n'+
	'						<hr>\n'+
	'						<table cellpadding="1" style="text-align: left">\n'+
	'							<tr>\n'+
	'								<th class="full_width">Доза введённая, ед</th>\n'+
	'								<td class="right">'+injectedDose+'</td>\n'+
	'							</tr>\n'+
	'							<tr>\n'+
	'								<th class="full_width">Доза рассчётная, ед</th>\n'+
	'								<td class="right">'+dose+'</td>\n'+
	'							</tr>\n'+
	'							<tr>\n'+
	'								<th class="full_width">Коррекция, г</th>\n'+
	'								<td class="right">'+dk+'</td>\n'+
	'							</tr>\n'+
	'						</table>\n'+
	'						<br>\n'+
	'						<div class="hint">\n'+
	'							<a href="#" onclick="switchMealInfo(false);">Таблица подбора</a> &gt;\n'+
	'						</div>';
}

function codeInfoNote(rec)
{
	return '' +
	"						Вы выбрали заметку.<br/><br/>\n<hr>" +
	"						Добавить новую запись:<br/><br/>\n" +
	"						<div>\n" +
	"							<button onclick=\"newBloodEditor()\" class=\"button_new_rec button_new_blood\" title=\"Добавить замер СК\"></button>\n" +
	"							<button onclick=\"newInsEditor()\" class=\"button_new_rec button_new_ins\" title=\"Добавить инъекцию\"></button>\n" +
	"							<button onclick=\"newNoteEditor()\" class=\"button_new_rec button_new_note\" title=\"Добавить заметку\"></button>\n" +
	"						</div>\n";
}

/* ================================ INFO PANEL UTILS ================================ */

function doMove(startHeight, targetHeight, startTime, duration) {
	var currentTime = new Date().getTime();

	var e = document.getElementById('filler');
	//var current = parseInt(getBefore(e.style.height, "px"));

	if (currentTime > startTime + duration)
	{
		e.style.height = targetHeight + "px";
		return;
	}

	var k = (currentTime - startTime) / duration;
	var height = startHeight * (1 - k) + targetHeight * k;

	e.style.height = height + "px";
	setTimeout("doMove("+startHeight + "," + targetHeight + "," + startTime + "," + duration + ")", 2);
}

function moveInfoBox(index)
{
	var targetHeight = 0;
	for (i = 0; i < index; i++)
	{
		targetHeight += document.getElementById("diaryRec_" + i).offsetHeight;
	}
	//document.getElementById('filler').setAttribute("style","height:" + targetHeight + "px");
	//document.getElementById('filler').style.height = targetHeight + "px";
	var startHeight = getBefore(document.getElementById('filler').style.height, "px");
	//alert(" --> " + result);
	doMove(startHeight, targetHeight, new Date().getTime(), 200);
}

function showInfoBox(index)
{
	currentID = index;
	moveInfoBox(index);

	if (index < 0)								infoblock.innerHTML = codeInfoDefault();
	else
	if (page.content[index].type == "blood")	infoblock.innerHTML = codeInfoBlood(page.content[index]);
	else
	if (page.content[index].type == "ins")		infoblock.innerHTML = codeInfoIns(page.content[index]);
	else
	if (page.content[index].type == "meal")		infoblock.innerHTML = codeInfoMeal(page.content[index]);
	else
	if (page.content[index].type == "note")		infoblock.innerHTML = codeInfoNote(page.content[index]); else
		infoblock.innerHTML = codeInfoDefault();

}

/* ================== DIARY PAGE METHODS ================== */

function modified(needResort)
{
	page.version++;
	page.timestamp = getCurrentTimestamp();
	if (needResort) page.content.sort(sortFunction);

	showPage();
	uploadPage();
}

function DiaryPage_addRecord(rec)
{
	page.content.push(rec);
	modified(true);
}

function DiaryPage_deleteRecord(index)
{
	page.content.splice(index, 1);
	modified(false);
}

function DiaryPage_changeTime(index, newTime)
{
	page.content[index].time = newTime;
	modified(true);
}

function DiaryPage_changeFoodMass(mealIndex, foodIndex, newMass)
{
	page.content[mealIndex].content[foodIndex].mass = newMass;
	modified(false);
}

function DiaryPage_removeFood(mealIndex, foodIndex)
{
	page.content[mealIndex].content.splice(foodIndex, 1);
	if (page.content[mealIndex].content.length == 0)
	{
		// удаляем также пустой приём пищи
		page.content.splice(mealIndex, 1);
	}
	modified(false);
}

function DiaryPage_getLastFinger()
{
	for (i = page.content.length - 1; i >= 0; i--)
	{
		if (page.content[i].type == "blood") return parseInt(page.content[i].finger);
	}
	return -1;
}

function DiaryPage_findRec(type, time, maxDist)
{
	var min = maxDist + 1;
	var rec = null;

	for (var i = 0; i < page.content.length; i++)
	{
		if (page.content[i].type == type)
		{
			var cur = Math.abs(page.content[i].time - time);
			if (cur < min)
			{
				min = cur;
				rec = page.content[i];
			}
		}
	}
	return rec;
}

function DiaryPage_findBlood(time, maxDist)
{
	return DiaryPage_findRec("blood", time, maxDist);
}

function DiaryPage_findIns(time, maxDist)
{
	return DiaryPage_findRec("ins", time, maxDist);
}

/* ================== FOODBASE METHODS ================== */

function findFoodByID(id)
{
	for (var i = 0; i < foodbase.food.length; i++)
	{
		if (foodbase.food[i].id == id)
		{
			//alert("findFoodByID(): founded, i=" + i);
			return foodbase.food[i];
		}
	}
//alert("findFoodByID(): NOT founded");
}

/* ================== EVENT HANDLERS ================== */

function newBloodEditor()
{
	var blood_rec = {
		"type": "blood",
		"time": "",
		"value" : "",
		"finger" : ""
	};

	blood_rec.finger = (DiaryPage_getLastFinger() + 1) % 10;
	var val = inputFloat("Введите значение СК ("+finger_hints[blood_rec.finger]+"):", "");

	if (val > -1)
	{
		blood_rec.time = getCurrentTime();
		blood_rec.value = String(val);
		DiaryPage_addRecord(blood_rec);
	}
}

function newInsEditor()
{
	var ins_rec = {
		"type": "ins",
		"time": "",
		"value" : ""
	};

	var val = inputFloat("Введите значение инъекции", "");

	//alert("newInsEditor(): val = " + val);

	if (val > -1)
	{
		ins_rec.time = getCurrentTime();
		ins_rec.value = String(val);
		DiaryPage_addRecord(ins_rec);
	}
}

function newMealEditor()
{
	var meal = {
		"type": "meal",
		"time": "",
		"content": [],
		"short": false
	};
	var newTime = inputTime("Введите время:", getCurrentTime());
	if ((newTime >= 0) && (newTime < 1440))
	{
		meal.time = newTime;
		DiaryPage_addRecord(meal);
	}
}

function newNoteEditor()
{
	var note = {
		"type": "note",
		"time": "",
		"text" : ""
	};
	var val = inputText("Введите заметку:");

	if (val != null)
	{
		note.text = val;
		note.time = getCurrentTime();
		DiaryPage_addRecord(note);
	}
}

function onDateChanged(datePicker)
{
	var date = datePicker.value;
	if (valid(date))
	{
		cur_date = new Date(Date.parse(date));
		//loadDoc(document.getElementById("diary_page"), "console.php?diary:download&dates=" + cur_date);
		//window.location='index.php?date=' + date;
		refreshCurrentPage();
	}
}

function onTimeClick(id)
{
	var index = getAfter(id, "_");
	var oldTime = page.content[index].time;
	var newTime = inputTime("Введите время:", oldTime);
	if ((newTime >= 0) && (newTime < 1440))
	{
		DiaryPage_changeTime(index, newTime);
	}
}

function onDeleteClick(id)
{
	var index = getAfter(id, "_");
	if (confirm("Удалить запись?"))
	{
		DiaryPage_deleteRecord(index);
	}
}

function onBloodClick(id)
{
	var index = getAfter(id, "_");
	var oldValue = page.content[index].value;
	var newValue = inputFloat("Введите значение СК:", oldValue);
	if (newValue > 0)
	{
		page.content[index].value = newValue;
		modified(false);
	}
}

function onInsClick(id)
{
	var index = getAfter(id, "_");
	var oldValue = page.content[index].value;
	var newValue = prompt("Введите дозу:", oldValue);
	if ((newValue != null) && (newValue > 0))
	{
		page.content[index].value = newValue;
		modified(false);
	}
}

function onNoteClick(id)
{
	var index = getAfter(id, "_");
	var oldText = page.content[index].text;
	var newText = prompt("Введите заметку:", oldText);
	if (newText != null)
	{
		page.content[index].text = newText;
		modified(false);
	}
}

function onRecordClick(id)
{
	showInfoBox(getAfter(id, "_"));
}

function onFoodMassClick(id)
{
	id = getAfter(id, "_");
	var mealIndex = getBefore(id, "_");
	var foodIndex = getAfter(id, "_");
	//alert("FoodClick, id = " + mealIndex + " / " + foodIndex);

	var oldMass = page.content[mealIndex].content[foodIndex].mass;
	var newMass = inputFloat("Введите массу:", oldMass);
	if (newMass != -1)
	{
		DiaryPage_changeFoodMass(mealIndex, foodIndex, newMass);
	// info panel updates automatically due to inherited clicking
	}
}

function onRemoveFoodClick(id)
{
	id = getAfter(id, "_");
	var mealIndex = getBefore(id, "_");
	var foodIndex = getAfter(id, "_");

	if (confirm("Удалить «" + page.content[mealIndex].content[foodIndex].name + "» ?"))
	{
		DiaryPage_removeFood(mealIndex, foodIndex);
	// info panel updates automatically due to inherited clicking
	}
}

function pushHistory(url)
{
	// "index.php?date=2013-04-01"
	// TODO: what does these parameters mean?
	var stateObj = {
		foo: "bar"
	};
	history.pushState(stateObj, "Компенсация", url);
}

function runScript(e, id)
{
	if (e.keyCode == 13)
	{
		console.log("runScript(): selID=" + selID + ", id=" + id);

		var food = findFoodByID(selID);
		var item = {};
		item.name = food.name;
		item.prots = food.prots;
		item.fats = food.fats;
		item.carbs = food.carbs;
		item.value = food.val;
		item.mass = e.target.value;

		if (item.mass >= 0)
		{
			//alert("Add " + ObjToSource(food) + " to meal #" + id);

			page.content[id].content.push(item);
			modified(false);

			// пост-настройка интерфейса
			showInfoBox(id);

			var component_combo = document.getElementById('mealcombo_' + id);
			var component_mass = e.target;

			component_mass.value = "";
			component_combo.value = "";
			component_combo.focus();
		}
		else
		{
			alert("Масса должна быть неотрицательной");
		}

		return false;
	}
}
