using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System;
using UnityEngine.AI;


public class MapScript : MonoBehaviour {

	// Use this for initialization
	private const int size = 21;
	private const int center = size / 2;
	private string[,] mapElements;
	private string[] objectTags = {"Empty", "Brick", "Stone", "Water", "Ivy"};
	private int[] randomIndex = {0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 3, 4};
	private static readonly System.Random getrandom = new System.Random();
	// EnemyTankScript enemyScript;

	public Vector3 basePosition;
	public NavMeshSurface surface;
	public int getMapSize()
	{
		return size;
	}
	void Start () {

		// enemyScript\
		mapElements = new string[size, size];
		generateMap();
		setNavMeshSize();
		surface.BuildNavMesh();
	}
	
	// Update is called once per frame
	void Update () {
		
	}

	private void createStandards()
	{
		// createEnemyTank();
		// createEnemyTank();
		// createEnemyTank();
		createSingleEnemy(size - 2, 1);
		createSingleEnemy(size - 1, center);  //EnemyTank
		createSingleEnemy(size - 2, size - 2);
		

		createSpaceForEnemy(0);
		createSpaceForEnemy(center - 1);
		createSpaceForEnemy(size - 3);
	}
	private void generateMap()
	{
		createStandards();
		createBase();
		createBorders();
	}
	
	private void createBase()
	{
		
		// GameObject gamePlatformObj = Instantiate(Resources.Load("GamePlatform"),  new Vector3(center, 0.5f, center), Quaternion.identity) as GameObject;
		// gamePlatformObj.transform.localScale = new Vector3(size, gamePlatformObj.transform.localScale.y, size*2);
		// mapElements[size - 1, center + 2] = "Tank";
		Vector3 wektor = new Vector3(center + 2f, 0, 4f);
		mapElements[0, center + 2] = "Tank";
		Instantiate(Resources.Load("Tank"),  new Vector3(center + 2, 0.5f, 1), Quaternion.Euler(0f, 180f, 0f));
		createSpaceForPlayer(center + 2);
		createSpaceForPlayer(center - 4);
		createSingle(1, center + 2, "Empty");
		createSingle(2, center + 2, "Empty");
		createSingle(center + 2, 1, "Empty");
		createSingle(1, center, "Stone");
		createSingle(1, center + 1, "Stone");
		createSingle(1, center - 1, "Stone");
		createSingle(2, center + 1, "Empty");
		createSingle(2, center - 1, "Empty");
		createSingle(2, center, "Empty");
		createSingle(2, center - 2, "Empty");
		createSingle(2, center + 2, "Empty");
		createSingle(0, center - 1, "Stone");
		createSingle(0, center + 1, "Stone");
		createHeartBase();


		for (int i = 0; i < size; ++i)
		{
			for (int j = 0; j < size; ++j)
			{
				if (mapElements[i, j] == null)
				{
					choseRandomElement(i, j);
				}
			}
		}

	}

	void choseRandomElement(int i, int j)
	{
		int random = getrandom.Next(randomIndex.Length); // creates a number between 
		int index = randomIndex[random];
		int caseSwitch = getrandom.Next(6);

		switch (caseSwitch)
      	{
			case 0:
				createSingle(i, j, objectTags[index]);
				break;
			case 1:
				createDoubleH(i, j, objectTags[index]);
				break;
			case 2:
				createDoubleV(i, j, objectTags[index]);
				break;
			case 3:
				createTripleLU(i, j, objectTags[index]);
				break;
			case 4:
				createTripleRU(i, j, objectTags[index]);
				break;
			case 5:
				createSquare(i, j, objectTags[index]);
				break;
			default:
				break;
      	}
	}
	
	void createSpaceForEnemy(int a)
	{
		for (int i = size - 3; i < size;++i)
		{
			for (int j = a; j < a + 3; ++j)
			{
				mapElements[i, j] = "Empty";
			}
		}
	}
	void createHeartBase()
	{
		GameObject g = Instantiate(Resources.Load("HeartBase"),  new Vector3(center, 0.0f, 0f), Quaternion.identity) as GameObject;
		mapElements[0, center] = "HeartBase";
		basePosition = g.transform.position;
	}

	void createSpaceForPlayer(int a)
	{
		for (int i = 0; i < 3;++i)
		{
			for (int j = a; j < a + 3; ++j)
			{
				mapElements[i, j] = "Empty";
			}
		}
	}

	void createBorders()
	{
		GameObject a = Instantiate(Resources.Load("Border"),  new Vector3(-1f, 1f, center), Quaternion.identity) as GameObject;
		a.transform.localScale = new Vector3(1f, 1f, size);
		GameObject b = Instantiate(Resources.Load("Border"),  new Vector3(size, 1f, center), Quaternion.identity) as GameObject;
		b.transform.localScale = new Vector3(1f, 1f, size);
		GameObject c = Instantiate(Resources.Load("Border"),  new Vector3(center, 1f, -1f), Quaternion.identity) as GameObject;
		c.transform.localScale = new Vector3(size + 2, 1f, 1f);
		GameObject d = Instantiate(Resources.Load("Border"),  new Vector3(center, 1f, size), Quaternion.identity) as GameObject;
		d.transform.localScale = new Vector3(size + 2, 1f, 1f);
	}

	void createSingle(int i, int j, String prefabName)
	{
		if (j < size &&  i < size && mapElements[i, j] == null)
		{
			if (prefabName != "Empty")
			{
				Instantiate(Resources.Load(prefabName),  new Vector3(j, 1f, i), Quaternion.identity);
			}
			mapElements[i, j] = prefabName;
		}
	}

	void createSingleEnemy(int i, int j)
	{
		if (j < size &&  i < size && mapElements[i, j] == null)
		{
			Instantiate(Resources.Load("EnemyTank"),  new Vector3(j, 0.5f, i), Quaternion.identity);
			mapElements[i, j] = "EnemyTank";
		}
	}

	public void createEnemyTank()
	{
		Vector3? position = EnemyTankScript.canCreateTank();
		
		if (position != null)
		{
			StartCoroutine(Wait(3f, position.GetValueOrDefault()));

		}
	}


	IEnumerator Wait(float duration, Vector3 pos)
    {
        //This is a coroutine
		Debug.Log("THIS IS FUCKING WAIT");
        yield return new WaitForSeconds(duration);   //Wait
		MonoBehaviour.Instantiate(Resources.Load("EnemyTank"),  pos, Quaternion.identity);
    }



	void createDoubleH(int i, int j, String prefabName)
	{
		createSingle(i, j, prefabName);
		createSingle(i, j + 1, prefabName);
	}

	void createDoubleV(int i, int j, String prefabName)
	{
		createSingle(i, j, prefabName);
		createSingle(i + 1, j, prefabName);
	}

	void createTripleRU(int i, int j, String prefabName)
	{
		createSingle(i, j, prefabName);
		createSingle(i + 1, j + 1, prefabName);
		createSingle(i, j + 1, prefabName);
	}

	void createTripleLU(int i, int j, String prefabName)
	{
		createSingle(i, j, prefabName);
		createSingle(i, j + 1, prefabName);
		createSingle(i + 1, j, prefabName);
	}

	void createSquare(int i, int j, String prefabName)
	{
		createSingle(i, j, prefabName);
		createSingle(i, j + 1, prefabName);
		createSingle(i + 1, j, prefabName);
		createSingle(i + 1, j + 1, prefabName);
	}

	void setNavMeshSize()
	{
		GameObject nav = GameObject.Find("NavMesh");
		nav.transform.localScale = new Vector3(size, 0f, size);
	}
}

