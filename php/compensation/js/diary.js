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
var dishbase = [];

var fdBase = [];
var selectedItem;
var currentID;

var foodbaseLoaded = false;
var dishbaseLoaded = false;

/* ================== STARTUP ACTIONS ================== */

refreshCurrentPage();
downloadKoofs();
downloadFoodbase();
downloadDishbase();

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

		page.content.sort(timeSortFunction);

		// debug only
		diary.innerHTML = "Sorted ok, rendering...";

		//document.getElementById("debug").innerHTML = ObjToSource(page);
		showPage();
	};

	var onFailure = function ()
	{
	//diary.innerHTML = "Не удалось загрузить страницу";
	}

	download(url, false, onSuccess, onFailure);
}

function downloadKoofs()
{
	var url = "console.php?koofs:download";

	var onSuccess = function(data)
	{
		if (data != "")
		{
			koofs = JSON.parse(data);
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

	download(url, true, onSuccess, onFailure);
}

function downloadFoodbase()
{
	foodbaseLoaded = false;

	var url = "console.php?foodbase:download";

	var onSuccess = function(data)
	{
		if (data != "")
		{
			data = getAfter(data, "\n");

			//data = data.replace(/&quot;/g, '\\"');
			data = data.replace(/&quot;/g, '');

			var dom = parseXml(data);
			var json = xml2json(dom, "");

			json = json.replace(/undefined/g, "").replace(/@/g, "");

			foodbase = JSON.parse(json).foods;

			// TODO: убрать?
			// простановка индексов
			for (var i = 0; i < foodbase.food.length; i++)
			{
				foodbase.food[i].id = i;
			}

			// сортировка - в будущем будет заменена на сортировку по релевантности
			foodbase.food.sort(nameSortFunction);

			foodbaseLoaded = true;
			if (foodbaseLoaded && dishbaseLoaded)
			{
				prepareComboList();
			}
			else
			{
				console.log("Foodbase downloaded, wait for dishbase...");
			}
		}
		else
		{
			foodbase = [];
		}
	};

	var onFailure = function ()
	{
		foodbase = [];
	}

	download(url, true, onSuccess, onFailure);
}

function downloadDishbase()
{
	dishbaseLoaded = false;

	var url = "console.php?dishbase:download";

	var onSuccess = function(data)
	{
		console.log("Dishbase loaded OK");
		console.log("Dishbase response data: '" + data + "'");

		if (data != "")
		{
			data = getAfter(data, "\n");

			//data = data.replace(/&quot;/g, '\\"');
			data = data.replace(/&quot;/g, '');

			//console.log("Dishbase XML (before parsing): " + data);

			var dom = parseXml(data);
			var json = xml2json(dom, "");

			json = json.replace(/undefined/g, "").replace(/@/g, "");

			//console.log("Dishbase JSON data: " + json);

			dishbase = JSON.parse(json).dishes;

			//console.log("Dishbase itself: " + ObjToSource(dishbase));

			// TODO: убрать?
			// простановка индексов
			for (var i = 0; i < dishbase.dish.length; i++)
			{
				dishbase.dish[i].id = i;
			}

			// сортировка - в будущем будет заменена на сортировку по релевантности
			dishbase.dish.sort(nameSortFunction);

			dishbaseLoaded = true;
			if (foodbaseLoaded && dishbaseLoaded)
			{
				prepareComboList();
			}
			else
			{
				console.log("Dishbase downloaded, wait for foodbase...");
			}
		}
		else
		{
			console.log("Dishbase data is empty");
			dishbase = [];
		}
	};

	var onFailure = function ()
	{
		console.log("Failed to load dishbase");
		dishbase = [];
	}

	download(url, true, onSuccess, onFailure);
}

function dishAsFood(dish)
{
	console.log("Converting dish '" + dish.name + "'...");alert

	var summProts = 0.0;
	var summFats = 0.0;
	var summCarbs = 0.0;
	var summVal = 0.0;
	var summMass = 0.0;

	for (i = 0; i < dish.item.length; i++)
	{
		console.log("adding item " + dish.item[i].name);
		summProts += dish.item.prots;
		summFats += dish.item.fats;
		summCarbs += dish.item.carbs;
		summVal += dish.item.val;
		summMass += dish.item.mass;
	}

	var dishMass;
	if ('mass' in dish)
	{
		dishMass = dish.mass;
	}
	else
	{
		dishMass = summMass;
	}

	var food = {};
	food.name = dish.name;
	food.prots = summProts / dishMass;
	food.fats = summFats / dishMass;
	food.carbs = summCarbs / dishMass;
	food.val = summVal / dishMass;

	return food;
}

function prepareComboList()
{
	fdBase = [];
	var item;

	// adding foods
	for (i = 0; i < foodbase.food.length; i++)
	{
		//console.log("Processing food " + i + " out of " + foodbase.food.length);
		item = {}
		item.value = foodbase.food[i].name;
		item.prots = foodbase.food[i].prots;
		item.fats = foodbase.food[i].fats;
		item.carbs = foodbase.food[i].carbs;
		item.val = foodbase.food[i].val;
		item.type = "food";
		fdBase.push(item);
	}

	// adding dishes
	for (i = 0; i < dishbase.dish.length; i++)
	{
		//alert("Processing dish " + i + " out of " + dishbase.dish.length);

		var cnv = dishAsFood(dishbase.dish[i]);

		item = {};
		item.value = cnv.name;
		item.prots = cnv.prots;
		item.fats = cnv.fats;
		item.carbs = cnv.carbs;
		item.val = cnv.val;
		item.type = "dish";
		fdBase.push(item);
	}

	//console.log("ComboList is ready: " + ObjToSource(fdBase));

	createHandlers();
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

	upload(url, request, true, onSuccess, onFailure);
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
	"								<td class=\"col_delete\"><button onclick=\"runScript(event,"+id+")\" title=\"Добавить\">+</button></td>\n" +
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
				selectedItem = ui.item;
				//console.log("Selected item: " + ObjToSource(ui));

				var t = 'mealmass_' + currentID;
				var mc = document.getElementById(t);
				console.log("Searching for component '" + t + "': " + mc);

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

	if ((index < 0)||(index >= page.content.length))	infoblock.innerHTML = codeInfoDefault();
	else
	if (page.content[index].type == "blood")			infoblock.innerHTML = codeInfoBlood(page.content[index]);
	else
	if (page.content[index].type == "ins")				infoblock.innerHTML = codeInfoIns(page.content[index]);
	else
	if (page.content[index].type == "meal")				infoblock.innerHTML = codeInfoMeal(page.content[index]);
	else
	if (page.content[index].type == "note")				infoblock.innerHTML = codeInfoNote(page.content[index]); else
		infoblock.innerHTML = codeInfoDefault();

}

/* ================== DIARY PAGE METHODS ================== */

function modified(needResort)
{
	page.version++;
	page.timestamp = getCurrentTimestamp();
	if (needResort) page.content.sort(timeSortFunction);

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

/* ================== EVENT HANDLERS ================== */

function newBloodEditor()
{
	var finger = (DiaryPage_getLastFinger() + 1) % 10;
	var val = inputFloat("Введите значение СК (" + finger_hints[finger] + "):", "");

	if (val > -1)
	{
		var blood_rec = {};
		blood_rec.type = "blood";
		blood_rec.finger = finger;
		blood_rec.time = getCurrentTime();
		blood_rec.value = String(val);
		DiaryPage_addRecord(blood_rec);
	}
}

function newInsEditor()
{
	var val = inputFloat("Введите значение инъекции", "");

	if (val > -1)
	{
		var ins_rec = {};
		ins_rec.type = "ins";
		ins_rec.time = getCurrentTime();
		ins_rec.value = String(val);
		DiaryPage_addRecord(ins_rec);
	}
}

function newMealEditor()
{
	var newTime = inputTime("Введите время:", getCurrentTime());
	if ((newTime >= 0) && (newTime < 1440))
	{
		var meal_rec = {};
		meal_rec.type = "meal";
		meal_rec.time = newTime;
		meal_rec.content = [];
		meal_rec.short = false;

		DiaryPage_addRecord(meal_rec);
	}
}

function newNoteEditor()
{
	var val = inputText("Введите заметку:");

	if (val != null)
	{
		var note_rec = {};
		note_rec.type = "note";
		note_rec.text = val;
		note_rec.time = getCurrentTime();
		DiaryPage_addRecord(note_rec);
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
		//console.log("runScript(): selectedItem=" + selectedItem + ", id=" + id);
		//console.log("selectedItem.value = " + selectedItem.value);
		//console.log("selectedItem.carbs = " + selectedItem.carbs);

		var item = {};
		item.name = selectedItem.value;
		item.prots = selectedItem.prots;
		item.fats = selectedItem.fats;
		item.carbs = selectedItem.carbs;
		item.value = selectedItem.val;
		item.mass = e.target.value;

		if (item.mass >= 0)
		{
			console.log("Add " + ObjToSource(item) + " to meal #" + id);
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
