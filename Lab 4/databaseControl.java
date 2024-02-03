import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.locks.ReentrantLock;

// User class represents information about a user
class User {
    int userID;
    String userName;
    String email;

    public User(int userID, String userName, String email) {
        this.userID = userID;
        this.userName = userName;
        this.email = email;
    }

    // getters
    public int getUserId() {
        return userID;
    }

    public String getUserName() {
        return userName;
    }

    public String getEmail() {
        return email;
    }

    // setters
    public void setUserId(int userID) {
        this.userID = userID;
    }

    public void setUserName(String userName) {
        this.userName = userName;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    // toString method to provide a string representation of the User object
    @Override
    public String toString() {
        return "User: { UserId = " + userID + ", UserName = '" + userName + "', email = '" + email + "' }";
    }
}

// Order class represents information about an order
class Order {
    int orderID;
    int userID;
    String product;
    int quantity;

    public Order(int orderID, int userID, String product, int quantity) {
        this.orderID = orderID;
        this.userID = userID;
        this.product = product;
        this.quantity = quantity;
    }

    // getters
    public int getUserId() {
        return userID;
    }

    public int getOderID() {
        return orderID;
    }

    public String getProduct() {
        return product;
    }

    public int getQuantity() {
        return quantity;
    }

    // setters
    public void setUserId(int userID) {
        this.userID = userID;
    }

    public void setOderID(int orderID) {
        this.orderID = orderID;
    }

    public void setProduct(String product) {
        this.product = product;
    }

    public void setQuantity(int quantity) {
        this.quantity = quantity;
    }

    // toString method to provide a string representation of the Order object
    @Override
    public String toString() {
        return "Order: { UserId = " + userID + ", OrderId = " + orderID + ", product = '" + product
                + "', quantity = " + quantity + " }";
    }
}

// DatabaseInitializer initializes sample data for users and orders
class DatabaseInitializer {
    // Method to initialize a list of users
    public static List<User> initializeUsers() {
        List<User> users = new ArrayList<>();
        users.add(new User(1, "aaa", "aaa@email.com"));
        users.add(new User(2, "bbb", "bbb@email.com"));
        users.add(new User(3, "ccc", "ccc@email.com"));
        return users;
    }

    // Method to initialize a list of orders
    public static List<Order> initializeOrders() {
        List<Order> orders = new ArrayList<>();
        orders.add(new Order(1, 1, "p1", 5));
        orders.add(new Order(2, 2, "p2", 4));
        orders.add(new Order(3, 3, "p2", 3));
        return orders;
    }
}

// DatabaseOperations provides methods to read user details and write orders with thread synchronization
class DatabaseOperations {
    private List<User> users;
    private List<Order> orders;
    private ReentrantLock lock;

    public DatabaseOperations(List<User> users, List<Order> orders) {
        this.users = users;
        this.orders = orders;
        this.lock = new ReentrantLock();
    }

    // Method to read user details with thread synchronization
    public synchronized User readUserDetails(int userID) {
        lock.lock();
        try {
            for (User user : users) {
                if (user.getUserId() == userID) {
                    return user;
                }
            }
            return null;
        } finally {
            lock.unlock();
        }
    }

    // Method to write orders with thread synchronization
    public synchronized void writeOrder(Order order) {
        lock.lock();
        try {
            orders.add(order);
            System.out.println("Writing new Order: " + order.toString());
        } finally {
            lock.unlock();
        }
    }
}

// Database class initializes users and orders and provides DatabaseOperations instance
class Database {
    private List<User> users;
    private List<Order> orders;

    public Database() {
        users = DatabaseInitializer.initializeUsers();
        orders = DatabaseInitializer.initializeOrders();
    }

    public DatabaseOperations getOperations() {
        return new DatabaseOperations(users, orders);
    }
}

// DatabaseTransaction represents a database transaction (read user details and write an order)
class DatabaseTransaction extends Thread {
    private DatabaseOperations dbOperations;
    private int userID;
    private String product;
    private int quantity;

    public DatabaseTransaction(DatabaseOperations dbOperations, int userID, String product, int quantity) {
        this.dbOperations = dbOperations;
        this.userID = userID;
        this.product = product;
        this.quantity = quantity;
    }

    // run method to execute the database transaction
    @Override
    public void run() {
        User user = dbOperations.readUserDetails(userID);
        System.out.println("Reading User details: " + user.toString());

        Order newOrder = new Order(4, userID, product, quantity);
        dbOperations.writeOrder(newOrder);
    }
}

// databaseControl class is the main class to demonstrate multithreaded database concurrency control
public class databaseControl {
    public static void main(String[] args) {
        Database database = new Database();

        // Creating multiple threads to simulate concurrent transactions
        for (int i = 0; i < 5; i++) {
            int userID = (int) (Math.random() * 3) + 1;
            String product = "p" + (char) ('A' + i);
            int quantity = (int) (Math.random() * 5) + 1;
            DatabaseTransaction transaction = new DatabaseTransaction(database.getOperations(), userID, product,
                    quantity);
            transaction.start();
        }
    }
}
