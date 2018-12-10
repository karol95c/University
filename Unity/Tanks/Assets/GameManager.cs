using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public static class GameManager {

	// Use this for initialization
	static EnemyTankScript enemyTankScript;
	static public void createEnemyTank()
	{
		Vector3? position = EnemyTankScript.canCreateTank();
		if (position != null)
		{
			// MonoBehaviour.StartCoroutine(Wait(2.0f, position.GetValueOrDefault()));
			// MonoBehaviour.Instantiate(Resources.Load("EnemyTank"),  position.GetValueOrDefault(), Quaternion.identity);

		}
	}

	static IEnumerator Wait(float duration, Vector3 pos)
    {
        //This is a coroutine
		Debug.Log("THIS IS FUCKING WAIT");
        yield return new WaitForSeconds(1f);   //Wait
		MonoBehaviour.Instantiate(Resources.Load("EnemyTank"),  pos, Quaternion.identity);
    }
}
