package org.bosik.compensation.persistence.common;

/**
 * Интерфейс обмена объектами
 * 
 * @author Bosik
 * 
 */
@Deprecated
public interface Interchangeable<T>
{
	/**
	 * Отправляет объект
	 * 
	 * @param object
	 *            Объект
	 */
	void postData(T object);

	/**
	 * Запрашивает объект
	 * 
	 * @return Объект
	 */
	T getData();

	/**
	 * Получает номер версии объекта
	 * 
	 * @return Номер версии
	 */
	int getVersion();
}